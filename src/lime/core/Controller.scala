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

  def universes: Seq[UniverseConfig] = this.children.collect { case m: Output => m }.flatMap(_.universes).toSeq

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
    println(s"IP address: $ip")
    println(s"MAC address: $mac")
    println(s"universes: ${universes.map(_.id).mkString(", ")}")

    new ClockingArea(systemClock) {

      // Global timeout tick counter (~1ms resolution at 125 MHz)
      /*val prescaler = Reg(UInt(17 bits)) init 0
      val tickCtr = Reg(UInt(12 bits)) init 0
      prescaler := prescaler + 1
      when(prescaler === 124999) {
        prescaler := 0
        tickCtr := tickCtr + 1
      }



      // Fair interleaver: path0 and path1 → single stream fanned out to all universes
      val interleaver = WriteInterleaver()
      interleaver.io.in0.valid := bridge.path0.io.sacnCmd.valid
      interleaver.io.in0.payload := bridge.path0.io.sacnCmd.payload
      interleaver.io.in1.valid := bridge.path1.io.sacnCmd.valid
      interleaver.io.in1.payload := bridge.path1.io.sacnCmd.payload

      val interleavedOut = interleaver.io.out.m2sPipe()

      // Sync: either path can trigger a buffer swap
      val syncPulse = bridge.path0.io.sacnSync || bridge.path1.io.sacnSync
      val syncAddr = Mux(bridge.path0.io.sacnSync, bridge.path0.io.sacnSyncAddr, bridge.path1.io.sacnSyncAddr)

      // Pack universes into UniverseBanks (all single-buffered for now; 4 per bank)
      val debugBits = for (group <- allUniverses.grouped(4).toSeq) yield {
        val configs = group.map(id => UniverseConfig(id, doubleBuffered = false)).toSeq
        val bank = UniverseBank(configs, writeClockDomain, boardClockDomain)
        bank.io.writeCmd.valid   := interleavedOut.valid
        bank.io.writeCmd.payload := interleavedOut.payload
        bank.io.sync             := syncPulse
        bank.io.syncAddr         := syncAddr
        bank.io.tickCtr          := tickCtr
        
        bank.io.read.addr        := 0
        
        bank.io.debugBit
      }
      // Pipeline the XOR reduction (6 bits -> 2 groups -> 1 bit)
      val stage1 = debugBits.grouped(3).map(g => RegNext(g.reduce(_ ^ _), init = False)).toSeq
      io.uart0 := RegNext(stage1.reduce(_ ^ _), init = False)
       */
    }
  }

  private def hash: Int = {
    scala.util.hashing.MurmurHash3.seqHash(universes.map(_.id).sorted)
  }
}
