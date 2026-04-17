package lime.core

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._

case class Mac(bytes: Seq[Option[Int]]) {
  require(bytes.length == 6 && bytes.forall(_.forall(b => b >= 0 && b <= 0xff)))
}

object Mac {
  def apply(s: String): Mac = {
    val parts = s.split(':')
    require(parts.length == 6, s"Expected XX:XX:XX:XX:XX:XX, got '$s'")
    Mac(parts.map(p => if (p.equalsIgnoreCase("xx")) None else Some(Integer.parseInt(p, 16))))
  }
}

case class Ip(bytes: Seq[Int]) {
  require(bytes.length == 4 && bytes.forall(b => b >= 0 && b <= 0xff))
}

object Ip {
  def apply(s: String): Ip = {
    val parts = s.split('.')
    require(parts.length == 4, s"Expected a.b.c.d, got '$s'")
    Ip(parts.map(_.toInt))
  }
}

abstract class Controller extends Component {
  def mac: Mac
  def ip: Ip

  def universes: Seq[Int]

  val io = new Bundle {
    val phy0 = RgmiiIo()
    val phy1 = RgmiiIo()
  }
  noIoPrefix()

  // PLL: 25 MHz board clock → 125 MHz system clock.
  private val pll = new Pll25to125
  pll.io.CLKI := ClockDomain.current.readClockWire
  pll.io.CLKFB := pll.io.CLKOP
  pll.io.RST := False
  pll.io.STDBY := False

  protected val systemClock = ClockDomain(
    clock = pll.io.CLKOP,
    config = ClockDomainConfig(resetKind = BOOT)
  )

  protected val bridge = new ClockingArea(systemClock) {
    val path0 = new BridgePath
    val path1 = new BridgePath
  }

  bridge.path0.io.rx <> io.phy0.rx
  bridge.path0.io.tx <> io.phy1.tx
  bridge.path1.io.rx <> io.phy1.rx
  bridge.path1.io.tx <> io.phy0.tx
}
