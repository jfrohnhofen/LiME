package lime.util

import spinal.core._
import spinal.lib._

case class Clocking() extends Component {
  val io = new Bundle {
    val clk = in Bool ()
    val sdramClk = out Bool ()
  }

  val pll = EHXPLLL()
  pll.io.CLKI := io.clk
  pll.io.CLKFB := pll.io.CLKOP
  pll.io.RST := False
  pll.io.STDBY := False

  io.sdramClk := pll.io.CLKOS2

  val network = ClockDomain(
    clock = pll.io.CLKOP,
    frequency = FixedFrequency(125 MHz),
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = ASYNC
    ),
    reset = ResetCtrl.asyncAssertSyncDeassert(
      input = pll.io.LOCK,
      clockDomain = ClockDomain(clock = pll.io.CLKOP)
    )
  )

  val system = ClockDomain(
    clock = pll.io.CLKOS,
    frequency = FixedFrequency(100 MHz),
    config = ClockDomainConfig(
      clockEdge = RISING,
      resetKind = ASYNC
    ),
    reset = ResetCtrl.asyncAssertSyncDeassert(
      input = pll.io.LOCK,
      clockDomain = ClockDomain(clock = pll.io.CLKOS)
    )
  )
}

// ECP5 PLL: 25 MHz → 125 MHz, 100 MHz, 100 MHz (-90 deg)
// VCO = (CLKI / CLKI_DIV) * CLKFB_DIV * CLKOP_DIV = 25 * 5 * 4 = 500 MHz
// CLKOP  = VCO / 4 = 125 MHz
// CLKOS  = VCO / 5 = 100 MHz
// CLKOS2 = VCO / 5 = 100 MHz (-90 deg)
protected case class EHXPLLL() extends BlackBox {
  val io = new Bundle {
    val CLKI = in Bool ()
    val CLKFB = in Bool ()
    val CLKOP = out Bool ()
    val CLKOS = out Bool ()
    val CLKOS2 = out Bool ()
    val LOCK = out Bool ()
    val RST = in Bool ()
    val STDBY = in Bool ()
  }
  noIoPrefix()

  // Frequency attributes for nextpnr
  addAttribute("FREQUENCY_PIN_CLKI", "25")
  addAttribute("FREQUENCY_PIN_CLKOP", "125")
  addAttribute("FREQUENCY_PIN_CLKOS", "100")
  addAttribute("FREQUENCY_PIN_CLKOS2", "100")

  addGeneric("CLKI_DIV", 1)
  addGeneric("CLKFB_DIV", 5)
  addGeneric("FEEDBK_PATH", "CLKOP")

  addGeneric("CLKOP_ENABLE", "ENABLED")
  addGeneric("CLKOP_DIV", 4)
  addGeneric("CLKOP_CPHASE", 2)
  addGeneric("CLKOP_FPHASE", 0)

  addGeneric("CLKOS_ENABLE", "ENABLED")
  addGeneric("CLKOS_DIV", 5)
  addGeneric("CLKOS_CPHASE", 3)
  addGeneric("CLKOS_FPHASE", 0)

  addGeneric("CLKOS2_ENABLE", "ENABLED")
  addGeneric("CLKOS2_DIV", 5)
  addGeneric("CLKOS2_CPHASE", 1)
  addGeneric("CLKOS2_FPHASE", 6) // (3.0 - 1.75) VCO cycles = 1.25 cycle shift forward = -90 deg
}
