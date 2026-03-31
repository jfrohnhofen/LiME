package lime.eth

import spinal.core._
import spinal.lib._

// ECP5 DDR input primitive
class IDDRX1F extends BlackBox {
  val io = new Bundle {
    val D    = in  Bool()
    val SCLK = in  Bool()
    val RST  = in  Bool()
    val Q0   = out Bool()
    val Q1   = out Bool()
  }
  mapClockDomain(clock = io.SCLK, reset = io.RST)
}

case class RgmiiRxIo() extends Bundle {
  val rxc   = in Bool()
  val rxd   = in Bits(4 bits)
  val rxCtl = in Bool()
}

class RgmiiRx extends Component {
  val io = new Bundle {
    val rgmii  = RgmiiRxIo()
    val output = master(Flow(Fragment(Bits(8 bits))))
  }

  // Inverted clock: IDDRX1F Q0 = high nibble, Q1 = low nibble — same byte, no alignment register needed
  val rxClockDomain = ClockDomain(
    clock  = io.rgmii.rxc,
    config = ClockDomainConfig(clockEdge = FALLING, resetKind = BOOT)
  )

  new ClockingArea(rxClockDomain) {
    val dataIddr = Array.fill(4)(new IDDRX1F)
    val ctlIddr  = new IDDRX1F

    for (i <- 0 until 4) {
      dataIddr(i).io.D := io.rgmii.rxd(i)
    }
    ctlIddr.io.D := io.rgmii.rxCtl

    // Q0 = high nibble (falling edge of rxc), Q1 = low nibble (rising edge of rxc)
    val highNibble = Bits(4 bits)
    val lowNibble  = Bits(4 bits)
    for (i <- 0 until 4) {
      highNibble(i) := dataIddr(i).io.Q0
      lowNibble(i)  := dataIddr(i).io.Q1
    }
    val rxDv  = ctlIddr.io.Q1  // RX_DV from rising edge, aligned with data
    val rxEnd = ctlIddr.io.Q0  // RX_DV from falling edge, goes low when frame ends

    io.output.valid    := rxDv
    io.output.fragment := highNibble ## lowNibble
    io.output.last     := rxDv && !rxEnd
  }
}
