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

case class Sdram() extends Bundle with IMasterSlave {
  val addr = Bits(11 bits)
  val ba = Bits(2 bits)
  val dq = Analog(Bits(32 bits))
  val ras_n = Bool()
  val cas_n = Bool()
  val we_n = Bool()
  val clk = Bool()

  override def asMaster(): Unit = {
    out(addr, ba, ras_n, cas_n, we_n, clk)
    inout(dq)
  }
}

class SdramTest extends Component {
  val io = new Bundle {
    val clk = in Bool ()
    val led_n = out Bool ()
    val sdram = master(Sdram())
  }
  noIoPrefix()

  val clocks = Clocking()
  clocks.io.clk := io.clk
  io.sdram.clk := clocks.io.sdramClk

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

    io.sdram.addr := ctrl.io.sdram.ADDR
    io.sdram.ba := ctrl.io.sdram.BA
    io.sdram.ras_n := ctrl.io.sdram.RASn
    io.sdram.cas_n := ctrl.io.sdram.CASn
    io.sdram.we_n := ctrl.io.sdram.WEn

    for (i <- 0 until 32) {
      val bb = BB()
      bb.io.I := ctrl.io.sdram.DQ.write(i)
      bb.io.T := !ctrl.io.sdram.DQ.writeEnable(i)
      ctrl.io.sdram.DQ.read(i) := bb.io.O
      bb.io.B <> io.sdram.dq(i)
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
