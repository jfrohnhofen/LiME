package lime.output

import spinal.core._

case class Hub75() extends Bundle {
  val a, b, c, d, e = Bool().assignDontCare()
  val clk = Bool().assignDontCare()
  val lat = Bool().assignDontCare()
  val oe = Bool().assignDontCare()
  val j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, j13, j14, j15, j16 = Hub75.Rgb().assignDontCare()

  def connectors: Seq[Hub75.Rgb] = Seq(j1, j2, j3, j4, j5, j6, j7, j8, j9, j10, j11, j12, j13, j14, j15, j16)
}

object Hub75 {
  case class Rgb() extends Bundle {
    val r0, g0, b0 = Bool()
    val r1, g1, b1 = Bool()

    def pins: Seq[Bool] = Seq(r0, g0, b0, r1, g1, b1)
  }
}
