package lime.output

import lime.core.{Controller, SdramReadPort}
import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// Driver for a chain of shift registers (74HC595 style: serial data, shift clock, latch)
// with single-bit LED outputs. One DMX channel byte per output; brightness comes from
// gamma correction (block-RAM LUT) followed by scanned PWM: the whole chain is refreshed
// once per scan and each output is on while its gamma value exceeds the bit-reversed scan
// phase, giving 256 PWM levels spread over 256 scans.
//
// Scans are padded to a fixed period so the PWM duty is exact. Channel 0 is shifted out
// first and therefore ends up at the *far end* of the chain.
case class ShiftRegisterLed(
    baseIndex: Int,
    startUniverse: Int,
    numOutputs: Int,
    syncUniverse: Option[Int],
    doubleBuffered: Boolean,
    sclkDiv: Int,
    gamma: Double,
    minPwmHz: Double
) extends Component
    with Output {
  require(numOutputs > 0)
  require(sclkDiv >= 4, "sclkDiv must be >= 4 for clean data/clock phasing")

  val uBytes = Output.universeBytes(numOutputs, 1, spanning = true)

  override def universes = uBytes.indices.map(i => UniverseConfig(startUniverse + i, syncUniverse, doubleBuffered))

  val fHz = ClockDomain.current.frequency.getValue.toDouble
  val scanPeriod = 2 * numOutputs * sclkDiv + 4 * sclkDiv // 100% fetch margin + latch

  val pwmHz = fHz / (scanPeriod.toDouble * 256)
  if (pwmHz < minPwmHz) {
    SpinalWarning(
      f"ShiftRegisterLed(startUniverse=$startUniverse): $numOutputs outputs give a PWM rate of " +
        f"only $pwmHz%.0f Hz (< $minPwmHz%.0f Hz) and will flicker. Use fewer outputs per chain " +
        f"or a smaller sclkDiv."
    )
  }

  val io = new Bundle {
    val data = out Bool ()
    val sclk = out Bool ()
    val latch = out Bool ()
    val read = master(SdramReadPort())
    val sel = in Vec (Bool(), uBytes.length)
  }

  override def read = io.read
  override def sel = io.sel

  val fetcher = UniverseFetcher(baseIndex, uBytes)
  fetcher.io.read <> io.read
  fetcher.io.sel := io.sel

  val lut = Mem(UInt(8 bits), 256)
  lut.init(Gamma.table(gamma, 0).map(U(_, 8 bits)))
  lut.addAttribute("ram_style", "block")

  val scanCnt = Reg(UInt(8 bits)) init 0
  val threshold = Reverse(scanCnt.asBits).asUInt

  val bytes = fetcher.io.bytes
  // Extra register after the BRAM: its ~5 ns clock-to-out plus the compare does not fit
  // one cycle
  val lutVal = RegNext(lut.readSync(bytes.fragment.asUInt, bytes.fire))
  val isLast = RegNextWhen(bytes.last, bytes.fire) init False

  val bitReg = RegInit(False)
  val divCnt = Reg(UInt(log2Up(sclkDiv) bits)) init 0
  val periodCnt = Reg(UInt(log2Up(scanPeriod + 1) bits)) init 0
  val dataReg = RegInit(False)
  val sclkReg = RegInit(False)
  val latchReg = RegInit(False)
  io.data := dataReg
  io.sclk := sclkReg
  io.latch := latchReg

  when(periodCnt =/= scanPeriod) { periodCnt := periodCnt + 1 }

  val fsm = new StateMachine {
    val FETCH = new State with EntryPoint
    val LUT = new State
    val APPLY = new State
    val BIT = new State
    val LATCH = new State
    val PAD = new State

    bytes.ready := isActive(FETCH)

    FETCH.whenIsActive {
      when(bytes.valid) { goto(LUT) }
    }

    LUT.whenIsActive { goto(APPLY) } // lutVal register captures the BRAM output

    APPLY.whenIsActive {
      bitReg := lutVal > threshold
      divCnt := 0
      goto(BIT)
    }

    BIT.whenIsActive {
      dataReg := bitReg
      sclkReg := divCnt >= sclkDiv / 2
      divCnt := divCnt + 1
      when(divCnt === sclkDiv - 1) {
        divCnt := 0
        sclkReg := False
        when(isLast) { goto(LATCH) } otherwise { goto(FETCH) }
      }
    }

    LATCH.whenIsActive {
      latchReg := True
      divCnt := divCnt + 1
      when(divCnt === sclkDiv - 1) {
        latchReg := False
        divCnt := 0
        goto(PAD)
      }
    }

    PAD.whenIsActive {
      when(periodCnt === scanPeriod) {
        periodCnt := 0
        scanCnt := scanCnt + 1
        goto(FETCH)
      }
    }
  }
}

object ShiftRegisterLed {
  def apply(
      dataPin: Bool,
      sclkPin: Bool,
      latchPin: Bool,
      startUniverse: Int,
      numOutputs: Int,
      syncUniverse: Option[Int] = None,
      doubleBuffered: Boolean = true,
      sclkDiv: Int = 10,
      gamma: Double = 2.2,
      minPwmHz: Double = 50
  ): ShiftRegisterLed = {
    val ctrl = Controller.current
    val numUniverses = Output.universeBytes(numOutputs, 1, spanning = true).length
    val base = ctrl.allocateUniverses(numUniverses)
    val area = new ClockingArea(ctrl.systemDomain) {
      val c = ShiftRegisterLed(base, startUniverse, numOutputs, syncUniverse, doubleBuffered, sclkDiv, gamma, minPwmHz)
    }
    dataPin := area.c.io.data
    sclkPin := area.c.io.sclk
    latchPin := area.c.io.latch
    area.c
  }
}
