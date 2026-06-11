package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

case class MacAddr(octets: Seq[Int]) {
  require(octets.length == 6 && octets.forall(o => o >= 0 && o <= 0xff))
  override def toString: String = octets.map("%02X".format(_)).mkString(":")
}

object MacAddr {
  def apply(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int): MacAddr = MacAddr(Seq(a, b, c, d, e, f))

  def apply(s: String): MacAddr = {
    val parts = s.split(':')
    MacAddr(parts.map(Integer.parseInt(_, 16)))
  }
}

case class IpAddr(octets: Seq[Int]) {
  require(octets.length == 4 && octets.forall(o => o >= 0 && o <= 0xff))
  override def toString: String = octets.mkString(".")
}

object IpAddr {
  def apply(a: Int, b: Int, c: Int, d: Int): IpAddr = IpAddr(Seq(a, b, c, d))

  def apply(s: String): IpAddr = {
    val parts = s.split('.')
    require(parts.length == 4, s"Expected a.b.c.d, got '$s'")
    IpAddr(parts.map(_.toInt))
  }
}

// All checksum/CRC work happens at elaboration time so the hardware only ever
// replays fully pre-computed frames (no checksum logic on the FPGA).
object Igmp {
  def multicastMac(group: IpAddr): MacAddr =
    MacAddr(Seq(0x01, 0x00, 0x5e, group.octets(1) & 0x7f, group.octets(2), group.octets(3)))

  // RFC 1071 internet checksum over big-endian 16-bit words
  private def checksum(bytes: Seq[Int]): Int = {
    val padded = if (bytes.length % 2 == 1) bytes :+ 0 else bytes
    var sum = padded.grouped(2).map { case Seq(a, b) => (a << 8) | b }.sum
    while ((sum >> 16) != 0) sum = (sum & 0xffff) + (sum >> 16)
    ~sum & 0xffff
  }

  // Ethernet FCS: CRC-32 (reflected, init/xorout 0xFFFFFFFF), transmitted LSByte first
  private def fcs(bytes: Seq[Int]): Seq[Int] = {
    val crc = new java.util.zip.CRC32
    crc.update(bytes.map(_.toByte).toArray)
    val v = crc.getValue
    Seq((v & 0xff).toInt, ((v >> 8) & 0xff).toInt, ((v >> 16) & 0xff).toInt, ((v >> 24) & 0xff).toInt)
  }

  // IGMPv2 Membership Report for `group` as a complete Ethernet frame (padded, FCS included)
  def membershipReport(srcMac: MacAddr, srcIp: IpAddr, group: IpAddr): Seq[Int] = {
    val eth = multicastMac(group).octets ++ srcMac.octets ++ Seq(0x08, 0x00)

    // IHL=6 (Router Alert option), TOS=0xC0, len=32, DF, TTL=1, proto=2 (IGMP)
    val ipNoCsum = Seq(0x46, 0xc0, 0x00, 0x20, 0x00, 0x00, 0x40, 0x00, 0x01, 0x02, 0x00, 0x00) ++
      srcIp.octets ++ group.octets ++ Seq(0x94, 0x04, 0x00, 0x00)
    val ipCsum = checksum(ipNoCsum)
    val ip = ipNoCsum.updated(10, ipCsum >> 8).updated(11, ipCsum & 0xff)

    val igmpNoCsum = Seq(0x16, 0x00, 0x00, 0x00) ++ group.octets
    val igmpCsum = checksum(igmpNoCsum)
    val igmp = igmpNoCsum.updated(2, igmpCsum >> 8).updated(3, igmpCsum & 0xff)

    val frame = eth ++ ip ++ igmp
    val padded = frame ++ Seq.fill(60 - frame.length)(0)
    padded ++ fcs(padded)
  }
}

// Replays a fixed set of pre-computed Ethernet frames (preamble/IPG are added by RgmiiTx,
// FCS must already be part of each frame). The whole set is sent once per interval; the
// first pass starts after firstDelayCycles so the PHY/switch has a chance to link up.
case class StaticPacketGen(frames: Seq[Seq[Int]], firstDelayCycles: BigInt, intervalCycles: BigInt) extends Component {
  require(frames.nonEmpty && frames.forall(_.nonEmpty))

  val io = new Bundle {
    val output = master Stream (Fragment(Byte))
  }

  val entries = frames.flatMap(f => f.zipWithIndex.map { case (b, i) => (b, i == f.length - 1) })
  val rom = Mem(Bits(9 bits), entries.map { case (b, last) => B((if (last) 0x100 else 0) | b, 9 bits) })

  val timerWidth = log2Up((firstDelayCycles max intervalCycles) + 1)
  val timer = Reg(UInt(timerWidth bits)) init firstDelayCycles
  val sending = Reg(Bool()) init False
  val idx = Reg(UInt(log2Up(entries.length) bits)) init 0

  // The ROM lives in block RAM (1 cycle read latency), so the address runs one entry
  // ahead of the output: advance moves the address whenever the output slot frees up.
  val rdValid = RegInit(False)
  val advance = sending && (!rdValid || io.output.ready)
  val entry = rom.readSync(idx, advance)
  val finalEntry = RegNextWhen(idx === entries.length - 1, advance) init False

  when(!sending) {
    when(timer === 0) {
      sending := True
    } otherwise {
      timer := timer - 1
    }
  }

  when(advance) {
    rdValid := True
    idx := Mux(idx === entries.length - 1, U(0, idx.getWidth bits), idx + 1)
  }

  io.output.valid := rdValid
  io.output.fragment := entry(7 downto 0)
  io.output.last := entry(8)

  // Stop once the final entry of the pass leaves; overrides the same-cycle prefetch
  // that may already have started reading the next pass.
  when(io.output.fire && finalEntry) {
    sending := False
    rdValid := False
    idx := 0
    timer := intervalCycles
  }
}
