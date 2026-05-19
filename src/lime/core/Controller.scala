package lime.core

import lime.output._
import lime.net._
import lime.util._

import spinal.core._
import spinal.lib._

abstract class Controller extends Component {
  val io = new Bundle {
    val phy0 = master(Rgmii())
    val phy1 = master(Rgmii())
    val hub75 = out(Hub75())

    def uart0 = hub75.j16.r0
    def uart1 = hub75.j16.g0
  }
  noIoPrefix()

  def mac: MacAddr = { val h = hash; MacAddr(0x02, 0x32, 0xcd, 0x32, (h >> 8) & 0xff, h & 0xff) }

  def ip: IpAddr = { val h = hash; IpAddr(169, 254, (h >> 8) & 0xff, h & 0xff) }

  // PLL: 25 MHz board clock → 125 MHz system clock
  private val pll = Pll25to125()
  pll.io.CLKI := ClockDomain.current.readClockWire
  pll.io.CLKFB := pll.io.CLKOP
  pll.io.RST := False
  pll.io.STDBY := False

  protected val systemClock = ClockDomain(
    clock = pll.io.CLKOP,
    config = ClockDomainConfig(resetKind = BOOT),
    frequency = FixedFrequency(125 MHz)
  )

  protected val bridge = new ClockingArea(systemClock) {
    val path0 = BridgePath()
    val path1 = BridgePath()
  }

  bridge.path0.io.rx <> io.phy0.rx
  bridge.path0.io.tx <> io.phy1.tx
  bridge.path0.io.uart <> io.uart0

  bridge.path1.io.rx <> io.phy1.rx
  bridge.path1.io.tx <> io.phy0.tx
  bridge.path1.io.uart <> io.uart1

  this.addPrePopTask { () =>
    val outputs = this.children.collect { case m: Output => m }
    println(s"IP address: $ip")
    println(s"MAC address: $mac")
    println(s"sACN universes: ${outputs.flatMap(_.sacnUniverses).mkString(", ")}")
  }

  private def hash: Int = {
    val universes = this.children.collect { case x: Output => x }.flatMap(_.sacnUniverses).sorted
    scala.util.hashing.MurmurHash3.seqHash(universes)
  }
}
