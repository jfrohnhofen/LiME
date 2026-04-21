package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

case class EthHeader() extends Bundle {
  val dstMac = Bits(48 bits)
  val srcMac = Bits(48 bits)
  val etherType = Bits(16 bits)
}

class EthRx extends Component {
  val io = new Bundle {
    val input = slave Flow (Fragment(Byte))
    val output = master Flow (Fragment(Byte))
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

  io.output.valid := io.input.valid && byteIdx >= 14
  io.output.fragment := io.input.fragment
  io.output.last := io.input.last && byteIdx >= 14

  // Assemble full EtherType combinatorially at byteIdx=13 to detect tags inline
  val fullEtherType = (etherType ## io.input.fragment).resize(16)
  val isTag = (byteIdx === 13) && (fullEtherType === 0x8100 || fullEtherType === 0x88a8)

  when(io.input.valid) {
    when(io.input.last) {
        byteIdx := 0
        skipCount := 0
    } elsewhen(skipCount > 0) {
      // Silently consume TCI bytes
      skipCount := skipCount - 1
    } otherwise {
      when(byteIdx < 6) { dstMac := (dstMac ## io.input.fragment).resized }
      when(byteIdx >= 6 && byteIdx < 12) { srcMac := (srcMac ## io.input.fragment).resized }
      when(byteIdx >= 12 && byteIdx < 14) { etherType := (etherType ## io.input.fragment).resized }

      when (isTag) {
        // Skip 2 TCI bytes, then re-parse EtherType from byteIdx=12
        byteIdx := 12
        skipCount := 2
      } elsewhen (byteIdx < 14) {
        byteIdx := byteIdx + 1
      }
    }
  }
}
