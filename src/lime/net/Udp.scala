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
    val input = slave(Flow(Fragment(Byte)))
    val output = master(Flow(Fragment(Byte)))
    val header = out(UdpHeader())
  }

  val byteIdx = Reg(UInt(4 bits)) init 0

  val srcPort = Reg(Bits(16 bits))
  val dstPort = Reg(Bits(16 bits))

  io.header.srcPort := srcPort
  io.header.dstPort := dstPort

  io.output.valid := io.input.valid && byteIdx >= PAYLOAD_OFFSET
  io.output.fragment := io.input.fragment
  io.output.last := io.input.last && byteIdx >= PAYLOAD_OFFSET

  when(io.input.valid) {
    when(io.input.last) {
      byteIdx := 0
    } otherwise {
      when(byteIdx >= SRC_PORT_OFFSET && byteIdx < DST_PORT_OFFSET) {
        srcPort := (srcPort ## io.input.fragment).resized
      }
      when(byteIdx >= DST_PORT_OFFSET && byteIdx < LENGTH_OFFSET) { dstPort := (dstPort ## io.input.fragment).resized }
      when(byteIdx < PAYLOAD_OFFSET) { byteIdx := byteIdx + 1 }
    }
  }
}
