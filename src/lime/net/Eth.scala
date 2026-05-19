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

case class EthHeader() extends Bundle {
  val dstMac = Bits(48 bits)
  val srcMac = Bits(48 bits)
  val etherType = Bits(16 bits)
}

case class EthRx() extends Component {
  final val VLAN_TAG = 0x8100
  final val SERVICE_TAG = 0x88a8

  final val DST_MAC_OFFSET = 0
  final val SRC_MAC_OFFSET = 6
  final val ETHER_TYPE_OFFSET = 12
  final val PAYLOAD_OFFET = 14

  val io = new Bundle {
    val input = slave(Flow(Fragment(Byte)))
    val output = master(Flow(Fragment(Byte)))
    val header = out(EthHeader())
  }

  val byteIdx = Reg(UInt(4 bits)) init 0
  val skipCount = Reg(UInt(2 bits)) init 0

  val dstMac = Reg(Bits(48 bits))
  val srcMac = Reg(Bits(48 bits))
  val etherType = Reg(Bits(16 bits))

  io.header.dstMac := dstMac
  io.header.srcMac := srcMac
  io.header.etherType := etherType

  io.output.valid := io.input.valid && byteIdx >= PAYLOAD_OFFET
  io.output.fragment := io.input.fragment
  io.output.last := io.input.last && byteIdx >= PAYLOAD_OFFET

  // Assemble full EtherType combinatorially at byteIdx=13 to detect tags inline
  val fullEtherType = (etherType ## io.input.fragment).resize(etherType.getWidth)
  val isTag = (byteIdx === PAYLOAD_OFFET - 1) && (fullEtherType === VLAN_TAG || fullEtherType === SERVICE_TAG)

  when(io.input.valid) {
    when(io.input.last) {
      byteIdx := 0
      skipCount := 0
    } elsewhen (skipCount > 0) {
      // Silently consume TCI bytes
      skipCount := skipCount - 1
    } otherwise {
      when(byteIdx >= DST_MAC_OFFSET && byteIdx < SRC_MAC_OFFSET) { dstMac := (dstMac ## io.input.fragment).resized }
      when(byteIdx >= SRC_MAC_OFFSET && byteIdx < ETHER_TYPE_OFFSET) { srcMac := (srcMac ## io.input.fragment).resized }
      when(byteIdx >= ETHER_TYPE_OFFSET && byteIdx < PAYLOAD_OFFET) {
        etherType := (etherType ## io.input.fragment).resized
      }

      when(isTag) {
        // Skip 2 TCI bytes, then re-parse EtherType from byteIdx=12
        byteIdx := ETHER_TYPE_OFFSET
        skipCount := PAYLOAD_OFFET - ETHER_TYPE_OFFSET
      } elsewhen (byteIdx < PAYLOAD_OFFET) {
        byteIdx := byteIdx + 1
      }
    }
  }
}
