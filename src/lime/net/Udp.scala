package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

object Udp {
  final val PROTOCOL_ID = 0x11
}

case class UdpHeader() extends Bundle {
  val srcPort = Bits(16 bits)
  val dstPort = Bits(16 bits)
}

case class UdpRx() extends Component {
  final val SRC_PORT_OFFSET = 0
  final val DST_PORT_OFFSET = 2
  final val LENGTH_OFFSET = 4
  final val PAYLOAD_OFFSET = 8

  val io = new Bundle {
    val input = slave(Packet())
    val output = master(Packet())
  }

  val header = Reg(UdpHeader()) init UdpHeader().getZero
  val byteIdx = Reg(UInt(4 bits)) init 0

  io.output.payload.fragment := io.input.payload.fragment
  io.output.payload.last := io.input.payload.last && byteIdx >= PAYLOAD_OFFSET
  io.output.payload.valid := io.input.payload.valid && byteIdx >= PAYLOAD_OFFSET
  io.output.eth := io.input.eth
  io.output.ip := io.input.ip
  io.output.udp := header

  when(io.input.payload.valid) {
    val payload = io.input.payload.fragment
    when(io.input.payload.last) {
      byteIdx := 0
    } otherwise {
      when(byteIdx >= SRC_PORT_OFFSET && byteIdx < DST_PORT_OFFSET) {
        header.srcPort := header.srcPort #<< payload
      }
      when(byteIdx >= DST_PORT_OFFSET && byteIdx < LENGTH_OFFSET) {
        header.dstPort := header.dstPort #<< payload
      }
      when(byteIdx < PAYLOAD_OFFSET) { byteIdx := byteIdx + 1 }
    }
  }
}
