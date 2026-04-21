package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

case class Ipv4Header() extends Bundle {
  val srcIp    = Bits(32 bits)
  val dstIp    = Bits(32 bits)
  val protocol = Bits(8 bits)
}

class Ipv4Rx extends Component {
  val io = new Bundle {
    val input  = slave  Flow(Fragment(Byte))
    val output = master Flow(Fragment(Byte))
    val header = out(Ipv4Header())
  }

  val byteIdx   = Reg(UInt(6 bits)) init 0
  val headerLen = Reg(UInt(6 bits)) init 20

  val srcIp    = Reg(Bits(32 bits))
  val dstIp    = Reg(Bits(32 bits))
  val protocol = Reg(Bits(8 bits))

  io.header.srcIp    := srcIp
  io.header.dstIp    := dstIp
  io.header.protocol := protocol

  io.output.valid    := io.input.valid && byteIdx >= headerLen
  io.output.fragment := io.input.fragment
  io.output.last     := io.input.last && byteIdx >= headerLen

  when(io.input.valid) {
    when(io.input.last) {
      byteIdx   := 0
      headerLen := 20
    } otherwise {
      when(byteIdx === 0) {
        headerLen := (io.input.fragment(3 downto 0).asUInt << 2).resized
      }
      when(byteIdx === 9)  { protocol := io.input.fragment }
      when(byteIdx >= 12 && byteIdx < 16) { srcIp := (srcIp ## io.input.fragment).resized }
      when(byteIdx >= 16 && byteIdx < 20) { dstIp := (dstIp ## io.input.fragment).resized }
      when(byteIdx < headerLen) { byteIdx := byteIdx + 1 }
    }
  }
}
