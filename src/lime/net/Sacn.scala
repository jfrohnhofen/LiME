package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

object Sacn {
  final val PORT = 5568

  object PacketType extends SpinalEnum {
    val Data, Sync = newElement()
  }
}

case class SacnHeader() extends Bundle {
  val packetType = Sacn.PacketType()
  val universe = UInt(16 bits)
  val priority = UInt(8 bits) // data packets only
  val syncAddress = UInt(16 bits)
  val seqNumber = UInt(8 bits)
  val options = Bits(8 bits)   // data packets only
  val propCount = UInt(10 bits) // data packets only; includes start code, so DMX channels = propCount - 1
}

// Parses sACN (E1.31) packets from a UDP payload byte stream.
// Validates all fixed constants and protocol fields; invalid packets are silently dropped.
//
// Data packets: DMX channel bytes streamed on io.output.
// Sync packets: io.sync fires for one cycle after the last byte; io.header.syncAddress valid.
//
// Byte offsets within the UDP payload:
//   [ 0- 1] Preamble Size      = 0x0010
//   [ 2- 3] Postamble Size     = 0x0000
//   [ 4-15] ACN Packet ID      = "ASC-E1.17\0\0\0"
//   [16-17] Root F&L           (flags nibble = 0x7)
//   [18-21] Root Vector        = 0x00000004 (Data) | 0x00000008 (Sync)
//   [22-37] CID                (16 bytes)
//   [38-39] Framing F&L        (flags nibble = 0x7)
//   [40-43] Framing Vector     = 0x00000002 (Data) | 0x00000001 (Sync)
//   --- Data ---
//   [44-107]  Source Name       (64 bytes, skipped)
//   [108]     Priority
//   [109-110] Sync Address
//   [111]     Sequence Number
//   [112]     Options
//   [113-114] Universe
//   [115-116] DMP F&L          (flags nibble = 0x7)
//   [117]     DMP Vector         = 0x02
//   [118]     Addr Type          = 0xa1
//   [119-120] First Prop Addr  = 0x0000
//   [121-122] Addr Increment   = 0x0001
//   [123-124] Property Count   (cross-validated against all three F&L lengths)
//   [125]     Start Code         = 0x00
//   [126+]    DMX channel data   → io.output
//   --- Sync ---
//   [44]    Sequence Number
//   [45-46] Sync Address       → io.sync fires one cycle after byte 46
case class SacnRx() extends Component {
  val io = new Bundle {
    val input = slave(Flow(Fragment(Byte)))
    val output = master(Flow(Fragment(Byte)))
    val sync = out(Bool())
    val header = out(SacnHeader())
  }

  import Sacn.PacketType._

  // Counts bytes within the current UDP payload packet (resets to 0 after last)
  val byteIdx = Reg(UInt(10 bits)) init 0

  // Cleared on any protocol violation; reset to True at the first byte of each new packet
  val isValid = Reg(Bool()) init True
  val isData = Reg(Bool()) init True // True for Data packets, False for Sync

  val universe = Reg(UInt(16 bits)) init 0
  val priority = Reg(UInt(8 bits)) init 0
  val syncAddr = Reg(UInt(16 bits)) init 0
  val seqNum = Reg(UInt(8 bits)) init 0
  val options = Reg(Bits(8 bits)) init 0
  val packetType = Reg(Sacn.PacketType()) init Data

  // F&L length fields (low 12 bits of each Flags & Length word)
  val rootLen    = Reg(UInt(12 bits)) init 0
  val framingLen = Reg(UInt(12 bits)) init 0
  val dmpLen     = Reg(UInt(12 bits)) init 0

  // propCount includes the start code byte, so DMX channels = propCount - 1
  val propCount  = Reg(UInt(10 bits)) init 0
  val lastDmxIdx = Reg(UInt(10 bits)) init 0  // byteIdx of last DMX byte

  io.header.packetType := packetType
  io.header.universe := universe
  io.header.priority := priority
  io.header.syncAddress := syncAddr
  io.header.seqNumber := seqNum
  io.header.options := options
  io.header.propCount := propCount

  // DMX data stream: valid Data packets, byte 126 through lastDmxIdx (= 124 + propCount)
  io.output.valid    := io.input.valid && isValid && isData && byteIdx >= 126 && byteIdx <= lastDmxIdx
  io.output.fragment := io.input.fragment
  io.output.last     := io.input.valid && isValid && isData && (byteIdx === lastDmxIdx || io.input.last)

  // Delayed one cycle so syncAddress captures the final byte before consumer reads it
  val syncReg = Reg(Bool()) init False
  syncReg := io.input.valid && io.input.last && isValid && !isData && byteIdx >= 46
  io.sync := syncReg

  val byte = io.input.fragment

  // ACN Packet Identifier: "ASC-E1.17\0\0\0"
  val ACN_ID = Seq(0x41, 0x53, 0x43, 0x2d, 0x45, 0x31, 0x2e, 0x31, 0x37, 0x00, 0x00, 0x00)

  when(io.input.valid) {
    // Optimistically assume valid at packet start; checks below may override to False.
    // This ordering is intentional: isValid/isData assignments here come first, so
    // any failing check later in this block wins (SpinalHDL last-assignment semantics).
    when(byteIdx === 0) {
      isValid := True
      isData := True
    }

    // Root Layer preamble: 0x0010
    when(byteIdx === 0 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 1 && byte.asUInt =/= 0x10) { isValid := False }

    // Root Layer postamble: 0x0000
    when(byteIdx === 2 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 3 && byte.asUInt =/= 0x00) { isValid := False }

    // ACN Packet Identifier (bytes 4-15)
    for ((b, i) <- ACN_ID.zipWithIndex) {
      when(byteIdx === (4 + i) && byte.asUInt =/= b) { isValid := False }
    }

    // Root Layer Flags & Length: check flags nibble, capture 12-bit length
    when(byteIdx === 16) {
      when(byte(7 downto 4).asUInt =/= 7) { isValid := False }
      rootLen(11 downto 8) := byte(3 downto 0).asUInt
    }
    when(byteIdx === 17) { rootLen(7 downto 0) := byte.asUInt }

    // Root Layer Vector (bytes 18-21): 0x00000004 (Data) or 0x00000008 (Sync)
    when(byteIdx === 18 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 19 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 20 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 21) {
      when(byte.asUInt =/= 0x04 && byte.asUInt =/= 0x08) { isValid := False }
      isData := (byte.asUInt === 0x04)
      when(byte.asUInt === 0x04) { packetType := Data }
        .otherwise { packetType := Sync }
    }

    // Framing Layer Flags & Length: check flags nibble, capture 12-bit length (common)
    when(byteIdx === 38) {
      when(byte(7 downto 4).asUInt =/= 7) { isValid := False }
      framingLen(11 downto 8) := byte(3 downto 0).asUInt
    }
    when(byteIdx === 39) { framingLen(7 downto 0) := byte.asUInt }

    // Framing Layer Vector (bytes 40-43): first 3 bytes are 0x00 for both packet types
    when(byteIdx === 40 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 41 && byte.asUInt =/= 0x00) { isValid := False }
    when(byteIdx === 42 && byte.asUInt =/= 0x00) { isValid := False }

    // Sync packet: fixed sizes rootLen=31 (47-16), framingLen=9 (47-38); vector[3]=0x01; fields
    when(!isData) {
      when(byteIdx === 40) {
        when(rootLen    =/= 31) { isValid := False }
        when(framingLen =/= 9)  { isValid := False }
      }
      when(byteIdx === 43 && byte.asUInt =/= 0x01) { isValid := False }
      when(byteIdx === 44) { seqNum                := byte.asUInt }
      when(byteIdx === 45) { syncAddr(15 downto 8) := byte.asUInt }
      when(byteIdx === 46) { syncAddr(7 downto 0)  := byte.asUInt }
    }

    // Data packet: vector[3]=0x02; source name (44-107) skipped; framing fields; DMP layer
    when(isData) {
      when(byteIdx === 43 && byte.asUInt =/= 0x02) { isValid := False }

      when(byteIdx === 108) { priority := byte.asUInt }
      when(byteIdx === 109) { syncAddr(15 downto 8) := byte.asUInt }
      when(byteIdx === 110) { syncAddr(7 downto 0) := byte.asUInt }
      when(byteIdx === 111) { seqNum := byte.asUInt }
      when(byteIdx === 112) { options := byte }
      when(byteIdx === 113) { universe(15 downto 8) := byte.asUInt }
      when(byteIdx === 114) { universe(7 downto 0) := byte.asUInt }

      // DMP Layer Flags & Length: check flags nibble, capture 12-bit length
      when(byteIdx === 115) {
        when(byte(7 downto 4).asUInt =/= 7) { isValid := False }
        dmpLen(11 downto 8) := byte(3 downto 0).asUInt
      }
      when(byteIdx === 116) { dmpLen(7 downto 0) := byte.asUInt }

      // DMP Layer Vector: 0x02
      when(byteIdx === 117 && byte.asUInt =/= 0x02) { isValid := False }

      // Address Type & Data Type: 0xa1 (relative addressing, unsigned DMX)
      when(byteIdx === 118 && byte.asUInt =/= 0xa1) { isValid := False }

      // First Property Address: 0x0000
      when(byteIdx === 119 && byte.asUInt =/= 0x00) { isValid := False }
      when(byteIdx === 120 && byte.asUInt =/= 0x00) { isValid := False }

      // Address Increment: 0x0001
      when(byteIdx === 121 && byte.asUInt =/= 0x00) { isValid := False }
      when(byteIdx === 122 && byte.asUInt =/= 0x01) { isValid := False }

      // Property Count (bytes 123-124)
      when(byteIdx === 123) { propCount(9 downto 8) := byte(1 downto 0).asUInt }
      when(byteIdx === 124) { propCount(7 downto 0) := byte.asUInt }

      // Start Code (byte 125): cross-validate F&L lengths now that propCount is registered,
      // compute lastDmxIdx, and check the null start code
      // rootLen = propCount + 109, framingLen = propCount + 87, dmpLen = propCount + 10
      when(byteIdx === 125) {
        when(byte.asUInt =/= 0x00) { isValid := False }
        when(rootLen    =/= (propCount + 109).resize(12)) { isValid := False }
        when(framingLen =/= (propCount + 87).resize(12))  { isValid := False }
        when(dmpLen     =/= (propCount + 10).resize(12))  { isValid := False }
        lastDmxIdx := (propCount + 124).resize(10)
      }

      // Bytes 126+: DMX channel data, forwarded via io.output
    }

    when(io.input.last) { byteIdx := 0 }
      .otherwise { byteIdx := byteIdx + 1 }
  }
}
