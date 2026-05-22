package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

object Ipv4 {
  final val ETHER_TYPE = 0x0800
}

case class Ipv4Header() extends Bundle {
  val srcIp = Bits(32 bits)
  val dstIp = Bits(32 bits)
  val protocol = Bits(8 bits)
}

case class Ipv4Rx() extends Component {
  final val HEADER_LEN_OFFSET = 0
  final val PROTOCOL_OFFSET = 9
  final val SRC_IP_OFFSET = 12
  final val DST_IP_OFFSET = 16
  final val PAYLOAD_OFFSET = 20

  val io = new Bundle {
    val input = slave(Packet())
    val output = master(Packet())
  }

  val header = Reg(Ipv4Header()) init Ipv4Header().getZero
  val byteIdx = Reg(UInt(6 bits)) init 0
  val headerLen = Reg(UInt(6 bits)) init PAYLOAD_OFFSET

  io.output.payload.fragment := io.input.payload.fragment
  io.output.payload.last := io.input.payload.last && byteIdx >= headerLen
  io.output.payload.valid := io.input.payload.valid && byteIdx >= headerLen
  io.output.eth := io.input.eth
  io.output.ip := header

  when(io.input.payload.valid) {
    val payload = io.input.payload.fragment
    when(io.input.payload.last) {
      byteIdx := 0
      headerLen := PAYLOAD_OFFSET
    } otherwise {
      when(byteIdx === HEADER_LEN_OFFSET) {
        headerLen := (payload.nibble(0).asUInt << 2).resized
      }
      when(byteIdx === PROTOCOL_OFFSET) { header.protocol := payload }
      when(byteIdx >= SRC_IP_OFFSET && byteIdx < DST_IP_OFFSET) {
        header.srcIp := header.srcIp #<< payload
      }
      when(byteIdx >= DST_IP_OFFSET && byteIdx < PAYLOAD_OFFSET) {
        header.dstIp := header.dstIp #<< payload
      }
      when(byteIdx < headerLen) { byteIdx := byteIdx + 1 }
    }
  }
}
