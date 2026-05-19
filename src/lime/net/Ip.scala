package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

object Ipv4 {
  final val ETHER_TYPE = 0x0800
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
    val input = slave(Flow(Fragment(Byte)))
    val output = master(Flow(Fragment(Byte)))
    val header = out(Ipv4Header())
  }

  val byteIdx = Reg(UInt(6 bits)) init 0
  val headerLen = Reg(UInt(6 bits)) init PAYLOAD_OFFSET

  val srcIp = Reg(Bits(32 bits))
  val dstIp = Reg(Bits(32 bits))
  val protocol = Reg(Bits(8 bits))

  io.header.srcIp := srcIp
  io.header.dstIp := dstIp
  io.header.protocol := protocol

  io.output.valid := io.input.valid && byteIdx >= headerLen
  io.output.fragment := io.input.fragment
  io.output.last := io.input.last && byteIdx >= headerLen

  when(io.input.valid) {
    when(io.input.last) {
      byteIdx := 0
      headerLen := PAYLOAD_OFFSET
    } otherwise {
      when(byteIdx === HEADER_LEN_OFFSET) {
        headerLen := (io.input.fragment(3 downto 0).asUInt << 2).resized
      }
      when(byteIdx === PROTOCOL_OFFSET) { protocol := io.input.fragment }
      when(byteIdx >= SRC_IP_OFFSET && byteIdx < DST_IP_OFFSET) { srcIp := (srcIp ## io.input.fragment).resized }
      when(byteIdx >= DST_IP_OFFSET && byteIdx < PAYLOAD_OFFSET) { dstIp := (dstIp ## io.input.fragment).resized }
      when(byteIdx < headerLen) { byteIdx := byteIdx + 1 }
    }
  }
}
