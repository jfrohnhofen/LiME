package lime.eth

import spinal.core._
import spinal.lib._

// ECP5 DDR input primitive.
class IDDRX1F extends BlackBox {
  val io = new Bundle {
    val D    = in  Bool()
    val SCLK = in  Bool()
    val RST  = in  Bool()
    val Q0   = out Bool()
    val Q1   = out Bool()
  }
  noIoPrefix()
  mapClockDomain(clock = io.SCLK)
}

// ECP5 programmable delay primitive.
class DELAYG(mode: String, value: Int) extends BlackBox {
  val io = new Bundle {
    val A = in  Bool()
    val Z = out Bool()
  }
  noIoPrefix()
  addGeneric("DEL_MODE", mode)
  addGeneric("DEL_VALUE", value)
}

case class RgmiiRxIo() extends Bundle {
  val rxc   = in Bool()
  val rxd   = in Bits(4 bits)
  val rxCtl = in Bool()
}

class RgmiiRx extends Component {
  val io = new Bundle {
    val rgmii  = RgmiiRxIo()
    val output = master(Flow(Bits(8 bits)))
  }

  val rxClockDomain = ClockDomain(
    clock  = io.rgmii.rxc,
    config = ClockDomainConfig(resetKind = BOOT)
  )

  new ClockingArea(rxClockDomain) {
    def ddrIn(d: Bool): (Bool, Bool) = {
      val delay = new DELAYG("SCLK_ALIGNED", 80)
      val iddr  = new IDDRX1F
      delay.io.A  := d
      iddr.io.D   := delay.io.Z
      iddr.io.RST := False
      (iddr.io.Q0, iddr.io.Q1)
    }

    val lowNibble  = Bits(4 bits)
    val highNibble = Bits(4 bits)
    for (i <- 0 until 4) {
      val (rising, falling) = ddrIn(io.rgmii.rxd(i))
      lowNibble(i)  := rising   // low nibble driven at rising edge
      highNibble(i) := falling  // high nibble driven at falling edge
    }

    // RX_CTL: rising = RX_DV, falling = RX_DV XOR RX_ERR
    val (rxDv, rxDvXorErr) = ddrIn(io.rgmii.rxCtl)

    io.output.valid    := rxDv && rxDvXorErr
    io.output.payload := highNibble ## lowNibble
  }
}
