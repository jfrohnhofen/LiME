package lime.net

import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

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
    val output = master(Flow(Fragment(Byte)))
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
      lowNibble(i) := rising
      highNibble(i) := falling
    }

    val (rxDv, rxDvXorErr) = ddrIn(io.rgmii.ctl)
    val valid = rxDv && rxDvXorErr
    val payload = highNibble ## lowNibble
    val prevValid = RegNext(valid, False)
    val prevPayload = RegNext(payload)
    val last = prevValid && !valid

    val fsm = new StateMachine {
      val IDLE = new State with EntryPoint
      val PREAMBLE = new State
      val INFRAME = new State

      IDLE.whenIsActive {
        when(valid) { goto(PREAMBLE) }
      }

      PREAMBLE.whenIsActive {
        when(!valid) { goto(IDLE) }
          .elsewhen(prevPayload === 0xd5) { goto(INFRAME) }
      }

      INFRAME.whenIsActive {
        when(last) { goto(IDLE) }
      }
    }

    io.output.valid := fsm.isActive(fsm.INFRAME)
    io.output.fragment := prevPayload
    io.output.last := last
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

  val IpgBytes = 12 // IPG: IEEE 802.3 requires 12 bytes (96 bits) minimum gap between frames.
  val PreambleBytes = 8 // 7 × 0x55 preamble + 0xd5 SFD.
  val ipgCounter = Reg(UInt(log2Up(IpgBytes) bits)) init (IpgBytes - 1)
  val preambleCounter = Reg(UInt(log2Up(PreambleBytes) bits)) init (PreambleBytes - 1)

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val PREAMBLE = new State
    val DATA = new State
    val IPG = new State

    IDLE.whenIsActive {
      when(io.input.valid) {
        preambleCounter := PreambleBytes - 1
        goto(PREAMBLE)
      }
    }

    PREAMBLE.whenIsActive {
      preambleCounter := preambleCounter - 1
      when(preambleCounter === 0) { goto(DATA) }
    }

    DATA.whenIsActive {
      when(io.input.fire && io.input.last) {
        ipgCounter := IpgBytes - 1
        goto(IPG)
      }
    }

    IPG.whenIsActive {
      ipgCounter := ipgCounter - 1
      when(ipgCounter === 0) { goto(IDLE) }
    }
  }

  io.input.ready := fsm.isActive(fsm.DATA)

  // Output clock: D0=1, D1=0 gives 50% duty cycle at the clock frequency.
  io.rgmii.clk := ddrOut(True, False)

  val txData =
    Mux(fsm.isActive(fsm.PREAMBLE), Mux(preambleCounter === 0, B(0xd5, 8 bits), B(0x55, 8 bits)), io.input.fragment)
  for (i <- 0 until 4) {
    io.rgmii.data(i) := ddrOut(txData(i), txData(i + 4))
  }

  // TX_CTL: rising = TX_EN, falling = TX_EN XOR TX_ERR. TX_ERR=0, so both edges = TX_EN.
  val txEn = fsm.isActive(fsm.PREAMBLE) || io.input.fire
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
