package lime.eth

import spinal.core._
import spinal.lib._

case class RgmiiRxIo() extends Bundle {
  val clk = in Bool ()
  val data = in Bits (4 bits)
  val ctl = in Bool ()
}

case class RgmiiTxIo() extends Bundle {
  val clk = out Bool ()
  val data = out Bits (4 bits)
  val ctl = out Bool ()
}

case class RgmiiIo() extends Bundle {
  val rx = RgmiiRxIo()
  val tx = RgmiiTxIo()
}

class RgmiiRx extends Component {
  val io = new Bundle {
    val rgmii = RgmiiRxIo()
    val output = master(Stream(Fragment(Bits(8 bits))))
  }

  val rxClockDomain = ClockDomain(
    clock = io.rgmii.clk,
    config = ClockDomainConfig(resetKind = BOOT)
  )

  new ClockingArea(rxClockDomain) {
    def ddrIn(d: Bool): (Bool, Bool) = {
      val delay = new DELAYG("SCLK_ALIGNED", 80)
      val iddr = new IDDRX1F
      delay.io.A := d
      iddr.io.D := delay.io.Z
      iddr.io.RST := False
      (iddr.io.Q0, iddr.io.Q1)
    }

    val lowNibble = Bits(4 bits)
    val highNibble = Bits(4 bits)
    for (i <- 0 until 4) {
      val (rising, falling) = ddrIn(io.rgmii.data(i))
      lowNibble(i) := rising // low nibble driven at rising edge
      highNibble(i) := falling // high nibble driven at falling edge
    }

    // RX_CTL: rising = RX_DV, falling = RX_DV XOR RX_ERR
    val (rxDv, rxDvXorErr) = ddrIn(io.rgmii.ctl)

    val byteValid   = rxDv && rxDvXorErr
    val prevValid   = RegNext(byteValid, False)
    val prevPayload = RegNext(highNibble ## lowNibble)

    val inFrame     = RegInit(False)
    val acceptFrame = RegInit(False)

    val last = prevValid && !byteValid

    // First byte of a frame: sample ready combinationally.
    // Subsequent bytes: use latched acceptFrame so ready fluctuations mid-frame are ignored.
    // Note: ready is a drop gate, not true backpressure — the PHY cannot be stalled.
    val accepting = Mux(inFrame, acceptFrame, io.output.ready)

    when(prevValid) {
      when(!inFrame) {        // frame start: latch admission decision
        inFrame     := True
        acceptFrame := io.output.ready
      }
      when(last) {            // frame end: reset for next frame
        inFrame     := False
        acceptFrame := False
      }
    }

    io.output.valid            := prevValid && accepting
    io.output.payload.fragment := prevPayload
    io.output.payload.last     := last
  }
}

class RgmiiTx extends Component {
  val io = new Bundle {
    val rgmii = RgmiiTxIo()
    val input = slave(Stream(Fragment(Bits(8 bits))))
  }

  def ddrOut(rising: Bool, falling: Bool): Bool = {
    val oddr = new ODDRX1F
    oddr.io.D0 := rising
    oddr.io.D1 := falling
    oddr.io.RST := False
    oddr.io.Q
  }

  // IPG: IEEE 802.3 requires 12 bytes (96 bits) minimum gap between frames.
  val IpgBytes = 12
  val ipgCounter = Reg(UInt(4 bits)) init 0
  val inIpg = ipgCounter =/= 0

  when(inIpg) {
    ipgCounter := ipgCounter - 1
  }

  when(io.input.fire && io.input.payload.last) {
    ipgCounter := IpgBytes
  }

  io.input.ready := !inIpg

  // Output clock: D0=1, D1=0 gives 50% duty cycle at the clock frequency.
  io.rgmii.clk := ddrOut(True, False)

  val txEn = io.input.fire
  val lowNibble  = io.input.payload.fragment(3 downto 0)
  val highNibble = io.input.payload.fragment(7 downto 4)
  for (i <- 0 until 4) {
    io.rgmii.data(i) := ddrOut(lowNibble(i), highNibble(i))
  }

  // TX_CTL: rising = TX_EN, falling = TX_EN XOR TX_ERR. TX_ERR=0, so both edges = TX_EN.
  io.rgmii.ctl := ddrOut(txEn, txEn)
}

// ECP5 DDR input primitive.
class IDDRX1F extends BlackBox {
  val io = new Bundle {
    val D = in Bool ()
    val SCLK = in Bool ()
    val RST = in Bool ()
    val Q0 = out Bool ()
    val Q1 = out Bool ()
  }
  noIoPrefix()

  mapClockDomain(clock = io.SCLK)
}

// ECP5 DDR output primitive.
class ODDRX1F extends BlackBox {
  val io = new Bundle {
    val D0 = in Bool ()
    val D1 = in Bool ()
    val SCLK = in Bool ()
    val RST = in Bool ()
    val Q = out Bool ()
  }
  noIoPrefix()

  mapClockDomain(clock = io.SCLK)
}

// ECP5 programmable delay primitive.
class DELAYG(mode: String, value: Int) extends BlackBox {
  val io = new Bundle {
    val A = in Bool ()
    val Z = out Bool ()
  }
  noIoPrefix()

  addGeneric("DEL_MODE", mode)
  addGeneric("DEL_VALUE", value)
}
