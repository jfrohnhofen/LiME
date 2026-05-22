package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

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
  final val PAYLOAD_OFFSET = 14

  val io = new Bundle {
    val input = slave(Flow(Fragment(Byte)))
    val output = master(Packet())
  }

  val header = Reg(EthHeader()) init (EthHeader().getZero)
  val byteIdx = Reg(UInt(4 bits)) init 0
  val skipCount = Reg(UInt(2 bits)) init 0

  io.output.payload.fragment := io.input.fragment
  io.output.payload.last := io.input.last && byteIdx >= PAYLOAD_OFFSET
  io.output.payload.valid := io.input.valid && byteIdx >= PAYLOAD_OFFSET
  io.output.eth := header

  // Assemble full EtherType combinatorially at byteIdx=13 to detect tags inline
  val fullEtherType = header.etherType #<< io.input.fragment
  val isTag = (byteIdx === PAYLOAD_OFFSET - 1) && (fullEtherType === VLAN_TAG || fullEtherType === SERVICE_TAG)

  when(io.input.valid) {
    when(io.input.last) {
      byteIdx := 0
      skipCount := 0
    } elsewhen (skipCount > 0) {
      // Silently consume TCI bytes
      skipCount := skipCount - 1
    } otherwise {
      when(byteIdx >= DST_MAC_OFFSET && byteIdx < SRC_MAC_OFFSET) {
        header.dstMac := header.dstMac #<< io.input.fragment
      }
      when(byteIdx >= SRC_MAC_OFFSET && byteIdx < ETHER_TYPE_OFFSET) {
        header.srcMac := header.srcMac #<< io.input.fragment
      }
      when(byteIdx >= ETHER_TYPE_OFFSET && byteIdx < PAYLOAD_OFFSET) {
        header.etherType := header.etherType #<< io.input.fragment
      }

      when(isTag) {
        // Skip 2 TCI bytes, then re-parse EtherType from byteIdx=12
        byteIdx := ETHER_TYPE_OFFSET
        skipCount := PAYLOAD_OFFSET - ETHER_TYPE_OFFSET
      } elsewhen (byteIdx < PAYLOAD_OFFSET) {
        byteIdx := byteIdx + 1
      }
    }
  }
}
