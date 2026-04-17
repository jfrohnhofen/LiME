package lime.util

import spinal.core._

// ECP5 PLL: 25 MHz → 125 MHz.
// CLKOP = CLKI * CLKFB_DIV / CLKI_DIV = 25 * 5 = 125 MHz
// VCO   = CLKOP * CLKOP_DIV = 625 MHz  (ECP5 range: 400–800 MHz)
// Verify or regenerate parameters with: ecppll -i 25 -o 125
class Pll25to125 extends BlackBox {
  val io = new Bundle {
    val CLKI = in Bool ()
    val CLKFB = in Bool ()
    val CLKOP = out Bool ()
    val LOCK = out Bool ()
    val RST = in Bool ()
    val STDBY = in Bool ()
  }
  noIoPrefix()

  setDefinitionName("EHXPLLL")

  addGeneric("CLKI_DIV", 1)
  addGeneric("CLKFB_DIV", 5)
  addGeneric("CLKOP_DIV", 5)
  addGeneric("CLKOP_ENABLE", "ENABLED")
  addGeneric("FEEDBK_PATH", "CLKOP")
}
