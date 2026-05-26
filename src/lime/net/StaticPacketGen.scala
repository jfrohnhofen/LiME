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

case class StaticPacketGen() extends Component {
  val io = new Bundle {
    val output = master Stream (Fragment(Byte))
  }

  val frameBytes: Seq[Int] = Seq(
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff, // dst MAC (broadcast)
    0x00, 0x0a, 0x35, 0x00, 0x01, 0x22, // src MAC
    0x08, 0x06, // EtherType: ARP
    0x00, 0x01, 0x08, 0x00, // HW type: Ethernet, proto: IPv4
    0x06, 0x04, 0x00, 0x01, // HW size, proto size, opcode: request
    0x00, 0x0a, 0x35, 0x00, 0x01, 0x22, // sender MAC
    0xc0, 0xa8, 0x01, 0x64, // sender IP: 192.168.1.100
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // target MAC
    0xc0, 0xa8, 0x01, 0x01, // target IP: 192.168.1.1
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // padding
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x9b, 0xf8, 0x2c, 0x5b // FCS
  )

  val frameLen = frameBytes.length
  val rom = Vec(frameBytes.map(b => B(b, 8 bits)))

  val sending = Reg(Bool()) init False
  val idx = Reg(UInt(log2Up(frameLen) bits)) init 0
  val gap = Reg(UInt(21 bits)) init 0 // 21 bits covers 1,250,000

  when(sending) {
    when(io.output.fire) {
      when(idx === frameLen - 1) {
        sending := False
        idx := 0
        gap := 1249999 // 10 ms at 125 MHz
      } otherwise {
        idx := idx + 1
      }
    }
  } otherwise {
    when(gap === 0) {
      sending := True
    } otherwise {
      gap := gap - 1
    }
  }

  io.output.valid := sending
  io.output.payload.fragment := rom(idx)
  io.output.payload.last := idx === frameLen - 1
}
