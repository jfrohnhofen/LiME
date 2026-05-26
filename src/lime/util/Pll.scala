package lime.util

import spinal.core._

case class Pll() extends Component {
  val io = new Bundle {
    val clk = in Bool()
    val clk_125Mhz = out Bool()
    val clk_100Mhz = out Bool()
    val clk_100Mhz_90deg = out Bool()
    val lock = out Bool()
  }  

  val ehxplll = EHXPLLL()
  ehxplll.io.CLKI := io.clk
  ehxplll.io.CLKFB := ehxplll.io.CLKOP
  ehxplll.io.RST := False
  ehxplll.io.STDBY := False
  
  io.clk_125Mhz := ehxplll.io.CLKOP
  io.clk_100Mhz := ehxplll.io.CLKOS
  io.clk_100Mhz_90deg := ehxplll.io.CLKOS2
  io.lock := ehxplll.io.LOCK
}

// ECP5 PLL: 25 MHz → 125 MHz, 100 MHz, 100 MHz (90 deg)
// VCO = (CLKI / CLKI_DIV) * CLKFB_DIV * CLKOP_DIV = 25 * 5 * 4 = 500 MHz
// CLKOP  = VCO / 4 = 125 MHz
// CLKOS  = VCO / 5 = 100 MHz
// CLKOS2 = VCO / 5 = 100 MHz
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
  addGeneric("CLKOS_CPHASE", 2)
  addGeneric("CLKOS_FPHASE", 0)

  addGeneric("CLKOS2_ENABLE", "ENABLED")
  addGeneric("CLKOS2_DIV", 5)
  addGeneric("CLKOS2_CPHASE", 3)
  addGeneric("CLKOS2_FPHASE", 2)
}
