package lime.core

import lime.output.UniverseConfig
import lime.util._
import spinal.core._
import spinal.lib._

// One beat of the sACN write stream produced by each BridgePath.
//
// Data packets become a fragment of beats carrying the DMX bytes in order; every beat
// carries the packet's universe and priority, the first beat has start set, and the last
// beat (fragment.last) has complete set iff the packet carried all announced bytes.
// Sync packets become a single beat with sync set and the sync address in universe.
case class SacnWrite() extends Bundle {
  val start = Bool()
  val sync = Bool()
  val complete = Bool()
  val universe = UInt(16 bits)
  val priority = UInt(8 bits)
  val data = Byte
}

case class SdramWriteCmd() extends Bundle {
  val address = UInt(21 bits)
  val data = Bits(32 bits)
  val mask = Bits(4 bits)
}

case class SdramReadPort() extends Bundle with IMasterSlave {
  val cmd = Stream(UInt(21 bits))
  val rsp = Flow(Bits(32 bits))

  override def asMaster(): Unit = { master(cmd); slave(rsp) }
}

// Stores incoming sACN DMX data in SDRAM and arbitrates between sources.
//
// Memory map: universe index i (position in `configs`) owns word addresses
// [i*256, i*256+255]: two 128-word (512-byte) buffer halves selected by io.sel(i).
// Single-buffered universes only ever use the low half.
//
// Per-universe sACN priority: a packet is accepted if its priority is >= the priority of
// the last accepted packet, or if no packet was accepted for timeoutMs (E1.31 network
// data loss timeout), so a higher-priority source overrides lower ones and lower ones
// take over again when it disappears.
//
// Double buffering: writes go to the back buffer half; on a complete packet the halves
// are swapped, unless the universe is bound to a sync universe, in which case the swap is
// deferred until a sync packet for that address arrives (per-output configurable).
case class UniverseStore(configs: Seq[UniverseConfig], timeoutMs: Int = 2500) extends Component {
  val n = configs.length
  require(n > 0, "UniverseStore needs at least one universe")
  require(configs.map(_.id).distinct.length == n, "duplicate universe ids")

  val io = new Bundle {
    val cmd = slave Stream (Fragment(SacnWrite()))
    val wr = master Stream (SdramWriteCmd())
    val sel = out Vec (Bool(), n)
  }

  val idxWidth = log2Up(n)

  // Per-universe state
  val prio = Vec.fill(n)(RegInit(U(0, 8 bits)))
  val age = Vec.fill(n)(RegInit(U(timeoutMs, log2Up(timeoutMs + 1) bits)))
  val sel = Vec.fill(n)(RegInit(False))
  val pending = Vec.fill(n)(RegInit(False))
  io.sel := sel

  // 1 ms tick driving the priority holdoff timeout
  val msTick = CounterFreeRun((ClockDomain.current.frequency.getValue.toDouble / 1000).round.toInt).willOverflow
  for (i <- 0 until n) {
    when(msTick && age(i) =/= timeoutMs) { age(i) := age(i) + 1 }
  }

  // Current packet state
  val writing = RegInit(False)
  val curIdx = Reg(UInt(idxWidth bits)) init 0
  val curSel = RegInit(False)
  val wordCnt = Reg(UInt(8 bits)) init 0 // bit 7 = 512-byte slot overflow guard
  val byteCnt = Reg(UInt(2 bits)) init 0
  val byteBuf = Vec.fill(3)(Reg(Byte))

  // Single-entry write buffer towards the SDRAM arbiter
  val wrValid = RegInit(False)
  val wrCmd = Reg(SdramWriteCmd())
  io.wr.valid := wrValid
  io.wr.payload := wrCmd
  when(io.wr.fire) { wrValid := False }

  // Stage 0 registers the beat together with its universe decode and accept decision;
  // computed inline they would sit in front of the whole write cone and break timing.
  // The decision uses prio/age as of one beat earlier, which only matters when two
  // sources race the same universe within a packet time and self-corrects on the next
  // packet, but keeps the deep compare/reduce trees out of the stage-1 register enables.
  private def stage0[T <: Data](v: T) = RegNextWhen(v, io.cmd.ready)

  val hits = stage0(Vec(configs.map(c => io.cmd.fragment.universe === c.id)))
  val accepts = stage0(Vec(configs.zipWithIndex.map { case (c, i) =>
    io.cmd.fragment.universe === c.id &&
    (io.cmd.fragment.priority >= prio(i) || age(i) === timeoutMs)
  }))
  val startAccept = stage0(Vec(configs.zipWithIndex.map { case (c, i) =>
    io.cmd.fragment.universe === c.id &&
    (io.cmd.fragment.priority >= prio(i) || age(i) === timeoutMs)
  }).reduceBalancedTree(_ || _))
  val cmd = io.cmd.m2sPipe()

  cmd.ready := !wrValid

  val beat = cmd.fragment

  // Shallow (registered hits against current sel), so evaluated in stage 1: a stale sel
  // would write into the half currently being displayed.
  val startSel = Vec((0 until n).map { i =>
    if (configs(i).doubleBuffered) hits(i) && !sel(i) else False // back buffer half, else low half
  }).reduceBalancedTree(_ || _)

  val idx = OHToUInt(hits).resize(idxWidth) // only feeds curIdx and address bits
  val cur = Vec.fill(n)(RegInit(False)) // one-hot universe of the packet being written

  when(cmd.fire) {
    when(beat.sync) {
      for ((c, i) <- configs.zipWithIndex; su <- c.syncUniverse) {
        when(beat.universe === su && pending(i)) {
          sel(i) := !sel(i)
          pending(i) := False
        }
      }
    } otherwise {
      val active = Mux(beat.start, startAccept, writing)
      val effIdx = Mux(beat.start, idx, curIdx)
      val effSel = Mux(beat.start, startSel, curSel)
      val effByteCnt = Mux(beat.start, U(0, 2 bits), byteCnt)
      val effWordCnt = Mux(beat.start, U(0, 8 bits), wordCnt)

      when(beat.start) {
        writing := startAccept
        curIdx := idx
        curSel := startSel
        wordCnt := 0
        byteCnt := 0
        for (i <- 0 until n) {
          cur(i) := hits(i)
          when(accepts(i)) {
            prio(i) := beat.priority
            age(i) := 0
          }
        }
      }

      when(active) {
        when(effByteCnt =/= 3) { byteBuf(effByteCnt.resized) := beat.data }

        val fullWord = effByteCnt === 3
        val emit = fullWord || (cmd.last && beat.complete)
        val inRange = !effWordCnt(7)

        when(emit && inRange) {
          wrValid := True
          wrCmd.address := (effIdx ## effSel ## effWordCnt(6 downto 0)).asUInt.resize(21)
          for (k <- 0 until 3) {
            wrCmd.data.byte(k) := Mux(effByteCnt === k, beat.data, byteBuf(k))
          }
          wrCmd.data.byte(3) := beat.data
          switch(effByteCnt) {
            is(0) { wrCmd.mask := B"0001" }
            is(1) { wrCmd.mask := B"0011" }
            is(2) { wrCmd.mask := B"0111" }
            is(3) { wrCmd.mask := B"1111" }
          }
        }

        when(fullWord) {
          byteCnt := 0
          when(inRange) { wordCnt := effWordCnt + 1 }
        } otherwise {
          byteCnt := effByteCnt + 1
        }

        for (i <- 0 until n if configs(i).doubleBuffered) {
          val isCur = Mux(beat.start, hits(i), cur(i))
          when(cmd.last && beat.complete && isCur) {
            if (configs(i).syncUniverse.isDefined) {
              pending(i) := True
            } else {
              sel(i) := !sel(i)
            }
          }
        }
      }

      when(cmd.last) { writing := False }
    }
  }
}
