package lime.core

import lime.output._
import lime.net._
import lime.util._

import spinal.core._
import spinal.lib._
import spinal.lib.memory.sdram.sdr.SdramCtrlCmd

// Base class for a board configuration. Subclasses only instantiate outputs (which map
// sACN universes onto pins); everything else - the two bridged Ethernet ports, the sACN
// receive pipeline, the SDRAM-backed universe store and the IGMP group registration -
// is wired up here once all outputs are known.
//
// The device's MAC and IP are derived by hashing the universe configuration, so every
// distinct configuration gets a stable, distinct link-local identity.
abstract class Controller extends Component {
  val io = new Bundle {
    val phy0 = master(Rgmii())
    val phy1 = master(Rgmii())
    val hub75 = out(Hub75())
    val sdram = master(Sdram())

    def uart0 = hub75.j16.r0
    def uart1 = hub75.j16.g0
  }
  noIoPrefix()

  def mac: MacAddr = { val h = hash; MacAddr(0x02, 0x32, 0xcd, 0x32, (h >> 8) & 0xff, h & 0xff) }

  // 169.254.1.0 .. 169.254.254.255 (RFC 3927 reserves the .0 and .255 /24s)
  def ip: IpAddr = { val h = hash; IpAddr(169, 254, 1 + (((h >> 8) & 0xff) % 254), h & 0xff) }

  def outputs: Seq[Output] = children.collect { case o: Output => o }.toSeq.sortBy(_.baseIndex)
  def universes: Seq[UniverseConfig] = outputs.flatMap(_.universes)

  // SDRAM universe slot allocation, claimed by outputs at construction time
  private var universeCount = 0
  def allocateUniverses(n: Int): Int = { val base = universeCount; universeCount += n; base }

  private[lime] val clocks = Clocking()
  clocks.io.clk := ClockDomain.current.readClockWire
  def systemDomain: ClockDomain = clocks.system

  protected val bridge = new ClockingArea(clocks.network) {
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
    val outs = outputs
    val configs = universes
    require(configs.nonEmpty, "a Controller needs at least one Output")
    require(configs.map(_.id).distinct.size == configs.size, s"duplicate universe ids: ${configs.map(_.id)}")
    // SacnRx validates the universe against the multicast MAC, whose 4th octet only holds
    // the low 7 bits of the universe high byte
    configs.foreach(c => require(c.id >= 1 && c.id <= 32767, s"universe id ${c.id} out of range 1..32767"))

    println(s"IP address:  $ip")
    println(s"MAC address: $mac")
    println(s"universes:   ${configs.map(_.id).mkString(", ")}")

    // IGMP membership reports for all data + sync universes, fully pre-computed,
    // sent on both ports shortly after startup and re-sent periodically so snooping
    // switches keep forwarding the groups to us.
    val groups = (configs.map(_.id) ++ configs.flatMap(_.syncUniverse)).distinct.sorted
    val frames = groups.map(g => Igmp.membershipReport(mac, ip, IpAddr(239, 255, g >> 8, g & 0xff)))

    val networkHz = clocks.network.frequency.getValue.toBigDecimal
    def cycles(seconds: Double) = (networkHz * seconds).toBigInt

    val net = new ClockingArea(clocks.network) {
      val igmp0 = StaticPacketGen(frames, cycles(0.5), cycles(10.0))
      val igmp1 = StaticPacketGen(frames, cycles(0.5), cycles(10.0))
    }
    bridge.path0.io.igmp << net.igmp0.io.output
    bridge.path1.io.igmp << net.igmp1.io.output

    // sACN write streams cross from the network to the system clock domain; overflow
    // (only possible under sustained line-rate sACN floods) drops beats, which the
    // store resynchronizes from via the start flag.
    val cc = Seq(bridge.path0, bridge.path1).map { path =>
      val fifo = StreamFifoCC(Fragment(SacnWrite()), 1024, clocks.network, clocks.system)
      fifo.io.push.valid := path.io.write.valid
      fifo.io.push.payload := path.io.write.payload
      fifo
    }

    val sys = new ClockingArea(clocks.system) {
      val merge = StreamArbiterFactory().roundRobin.fragmentLock.build(Fragment(SacnWrite()), 2)
      merge.io.inputs(0) << cc(0).io.pop
      merge.io.inputs(1) << cc(1).io.pop

      val store = UniverseStore(configs)
      store.io.cmd <-/< merge.io.output

      val idWidth = log2Up(outs.size) max 1
      def cmdType = SdramCtrlCmd(M12L64322A.layout, UInt(idWidth bits))

      val sdramCtrl = new SdramCtrl(UInt(idWidth bits))
      sdramCtrl.io.sdramClk := clocks.io.sdramClk

      // One shared command path: the store's writes plus one read client per output
      val wrCmd = store.io.wr.translateWith {
        val c = cmdType
        c.address := store.io.wr.address
        c.write := True
        c.data := store.io.wr.data
        c.mask := store.io.wr.mask
        c.context := 0
        c
      }

      val rdCmds = outs.zipWithIndex.map { case (o, i) =>
        o.read.cmd
          .translateWith {
            val c = cmdType
            c.address := o.read.cmd.payload
            c.write := False
            c.data := 0
            c.mask := 0
            c.context := i
            c
          }
          .m2sPipe() // arbiter inputs come straight from registers
      }

      // Arbitrate read clients as a pipelined tree: a flat arbiter over dozens of
      // clients would not close timing at 100 MHz
      def arbitrate(inputs: Seq[Stream[SdramCtrlCmd[UInt]]]): Stream[SdramCtrlCmd[UInt]] = {
        if (inputs.size == 1) return inputs.head
        val arb = StreamArbiterFactory().roundRobin.transactionLock.build(cmdType, inputs.size)
        for ((in, i) <- inputs.zipWithIndex) arb.io.inputs(i) << in
        arb.io.output
      }
      val rdGroups = rdCmds.grouped(6).map(g => arbitrate(g).s2mPipe().m2sPipe()).toSeq
      sdramCtrl.io.cmd << arbitrate(wrCmd +: rdGroups)

      // Reads come back in command order; broadcast through a register and route by
      // context (writes produce no response)
      sdramCtrl.io.rsp.ready := True
      val rsp = sdramCtrl.io.rsp.toFlowFire.stage()
      for ((o, i) <- outs.zipWithIndex) {
        o.read.rsp.valid := rsp.valid && rsp.context === i
        o.read.rsp.payload := rsp.data
      }

      for (o <- outs; j <- o.sel.indices) {
        o.sel(j) := store.io.sel(o.baseIndex + j)
      }
    }

    io.sdram <> sys.sdramCtrl.io.sdram
  }

  private def hash: Int = {
    scala.util.hashing.MurmurHash3.seqHash(universes.map(_.id).sorted)
  }
}

object Controller {
  def current: Controller = Component.current match {
    case c: Controller => c
    case _             => SpinalError("Outputs must be instantiated directly inside a Controller subclass")
  }
}
