package testing
import lime.util._

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import spinal.lib.fsm._
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr._

// ECP5 Bidirectional Buffer
case class BB() extends BlackBox {
  val io = new Bundle {
    val I = in Bool ()
    val T = in Bool ()
    val O = out Bool ()
    val B = inout(Analog(Bool()))
  }
  noIoPrefix()
  setDefinitionName("BB") // Ensure tool recognizes it as a primitive
}

object M12L64322A {
  def layout = SdramLayout(
    generation = SdramGeneration.SDR,
    bankWidth = 2,
    columnWidth = 8,
    rowWidth = 11,
    dataWidth = 32
  )

  // Conservative timings for better stability
  def timings = SdramTimings(
    bootRefreshCount = 8,
    tPOW = 200 us,
    tREF = 64 ms, // Total refresh period
    tRC = 70 ns, // Row cycle time
    tRFC = 70 ns, // Refresh cycle time
    tRAS = 48 ns, // Row active time
    tRP = 20 ns, // Row precharge time
    tRCD = 20 ns, // Row to column delay
    cMRD = 2, // Mode register delay
    tWR = 20 ns, // Write recovery time
    cWR = 2 // Write recovery cycles
  )
}

class SdramTest extends Component {
  val io = new Bundle {
    val clk = in Bool ()
    val led_n = out Bool ()

    val sdram_ADDR = out Bits (11 bits)
    val sdram_BA = out Bits (2 bits)
    val sdram_DQ = inout(Analog(Bits(32 bits)))
    val sdram_RASn = out Bool ()
    val sdram_CASn = out Bool ()
    val sdram_WEn = out Bool ()
    val sdram_CLK = out Bool ()
  }
  noIoPrefix()

  val clocks = Clocking()
  clocks.io.clk := io.clk
  io.sdram_CLK := clocks.io.sdramClk

  val system = new ClockingArea(clocks.system) {
    val ctrl = SdramCtrl(
      M12L64322A.layout,
      M12L64322A.timings,
      CAS = 3,
      contextType = NoData()
    )

    ctrl.io.bus.cmd.valid := False
    ctrl.io.bus.cmd.address := 0
    ctrl.io.bus.cmd.write := False
    ctrl.io.bus.cmd.data := 0
    ctrl.io.bus.cmd.mask := 0xf
    ctrl.io.bus.rsp.ready := True

    io.sdram_ADDR := ctrl.io.sdram.ADDR
    io.sdram_BA := ctrl.io.sdram.BA
    io.sdram_RASn := ctrl.io.sdram.RASn
    io.sdram_CASn := ctrl.io.sdram.CASn
    io.sdram_WEn := ctrl.io.sdram.WEn

    for (i <- 0 until 32) {
      val bb = BB()
      bb.io.I := ctrl.io.sdram.DQ.write(i)
      bb.io.T := !ctrl.io.sdram.DQ.writeEnable(i)
      ctrl.io.sdram.DQ.read(i) := bb.io.O
      bb.io.B <> io.sdram_DQ(i)
    }

    val testPass = Reg(Bool()) init False
    val testFail = Reg(Bool()) init False

    // Testing logic
    val address = Reg(UInt(21 bits)) init 0
    val counter = Reg(UInt(32 bits)) init 0

    val fsm = new StateMachine {
      val WRITE, READ, DONE, ERROR = new State
      setEntry(WRITE)
      WRITE.whenIsActive {
        ctrl.io.bus.cmd.valid := True
        ctrl.io.bus.cmd.write := True
        ctrl.io.bus.cmd.address := address
        ctrl.io.bus.cmd.data := address.asBits.resized
        when(ctrl.io.bus.cmd.ready) {
          address := address + 1
          when(address === address.maxValue) {
            address := 0
            goto(READ)
          }
        }
      }

      READ.whenIsActive {
        ctrl.io.bus.cmd.valid := True
        ctrl.io.bus.cmd.write := False
        ctrl.io.bus.cmd.address := address
        when(ctrl.io.bus.cmd.ready) {
          address := address + 1
          when(address === address.maxValue) {
            goto(DONE)
          }
        }
        when(ctrl.io.bus.rsp.valid) {
          // Check if data matches written pattern
          when(ctrl.io.bus.rsp.data =/= counter.asBits.resized) {
            goto(ERROR)
          }
          counter := counter + 1
        }
      }

      DONE.whenIsActive(testPass := True)
      ERROR.whenIsActive(testFail := True)
    }

    val blinker = Reg(UInt(30 bits)) init 0
    blinker := blinker + 1

    val isDone = testPass || testFail
    io.led_n := Mux(testPass, blinker(24), True)
  }
}
