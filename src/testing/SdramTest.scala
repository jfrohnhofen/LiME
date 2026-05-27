package testing

import lime.core._
import lime.util._

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import spinal.lib.fsm._
import spinal.lib.com.uart._

class SdramTest extends Component {
  val io = new Bundle {
    val clk = in Bool ()
    val led_n = out Bool ()
    val sdram = master(Sdram())
    val uart = out Bool ()
  }
  noIoPrefix()
  io.uart.setName("hub75_j16_r0")

  val clocks = Clocking()
  clocks.io.clk := io.clk

  val system = new ClockingArea(clocks.system) {
    val ctrl = new SdramCtrl()
    ctrl.io.sdramClk := clocks.io.sdramClk
    io.sdram <> ctrl.io.sdram

    ctrl.io.cmd.valid := False
    ctrl.io.cmd.address := 0
    ctrl.io.cmd.write := False
    ctrl.io.cmd.data := 0
    ctrl.io.cmd.mask := 0xf
    ctrl.io.rsp.ready := True

    // UART Reporting
    val uartCtrl = UartCtrl(uartConfig)
    io.uart := uartCtrl.io.uart.txd
    uartCtrl.io.uart.rxd := True
    uartCtrl.io.write.valid := False
    uartCtrl.io.write.payload := 0

    // Performance measurement
    val timer = Reg(UInt(32 bits)) init 0
    val totalWriteCycles = Reg(UInt(32 bits)) init 0
    val totalReadCycles = Reg(UInt(32 bits)) init 0
    val testPass = Reg(Bool()) init False
    val testFail = Reg(Bool()) init False

    val address = Reg(UInt(21 bits)) init 0
    val counter = Reg(UInt(32 bits)) init 0
    val outOfOrder = Reg(Bool()) init False

    def mangleAddress(addr: UInt): UInt = {
      val reversed = addr.asBits.resized.reversed
      outOfOrder ? reversed.asUInt | addr
    }

    val fsm = new StateMachine {
      val WAIT_INIT = new State with EntryPoint
      val INIT, WRITE, READ, REPORT, NEXT_MODE, DONE, ERROR = new State

      WAIT_INIT.whenIsActive {
        timer := timer + 1
        when(timer === 30000) {
          goto(INIT)
        }
      }

      INIT.whenIsActive {
        timer := 0
        address := 0
        counter := 0
        goto(WRITE)
      }

      WRITE.whenIsActive {
        timer := timer + 1
        ctrl.io.cmd.valid := True
        ctrl.io.cmd.write := True
        ctrl.io.cmd.address := mangleAddress(address)
        ctrl.io.cmd.data := address.asBits.resized
        when(ctrl.io.cmd.ready) {
          address := address + 1
          when(address === 10000) {
            totalWriteCycles := timer
            address := 0
            timer := 0
            goto(READ)
          }
        }
      }

      READ.whenIsActive {
        timer := timer + 1
        when(address =/= 10000) {
          ctrl.io.cmd.valid := True
          ctrl.io.cmd.write := False
          ctrl.io.cmd.address := mangleAddress(address)
          when(ctrl.io.cmd.ready) {
            address := address + 1
          }
        }
        when(ctrl.io.rsp.valid) {
          when(ctrl.io.rsp.data =/= counter.asBits.resized) {
            goto(ERROR)
          }
          counter := counter + 1
          when(counter === 9999) {
            totalReadCycles := timer
            goto(REPORT)
          }
        }
      }

      val reportIndex = Reg(UInt(8 bits)) init 0
      val reportData = Reg(Bits(8 bits)) init 0

      REPORT.whenIsActive {
        uartCtrl.io.write.valid := True
        when(uartCtrl.io.write.ready) {
          reportIndex := reportIndex + 1
          switch(reportIndex) {
            is(0) { reportData := (outOfOrder ? B(88, 8 bits) | B(83, 8 bits)) } // 'X' or 'S'
            is(1) { reportData := 32 } // space
            is(2) { reportData := 87 } // 'W'
            is(3) { reportData := 58 } // ':'
            is(4) { reportData := totalWriteCycles(31 downto 24).asBits }
            is(5) { reportData := totalWriteCycles(23 downto 16).asBits }
            is(6) { reportData := totalWriteCycles(15 downto 8).asBits }
            is(7) { reportData := totalWriteCycles(7 downto 0).asBits }
            is(8) { reportData := 32 } // space
            is(9) { reportData := 82 } // 'R'
            is(10) { reportData := 58 } // ':'
            is(11) { reportData := totalReadCycles(31 downto 24).asBits }
            is(12) { reportData := totalReadCycles(23 downto 16).asBits }
            is(13) { reportData := totalReadCycles(15 downto 8).asBits }
            is(14) { reportData := totalReadCycles(7 downto 0).asBits }
            is(15) { reportData := 10 } // \n
            is(16) { reportData := 13 } // \r
            default {
              reportIndex := 0
              goto(NEXT_MODE)
            }
          }
        }
        uartCtrl.io.write.payload := reportData
      }

      NEXT_MODE.whenIsActive {
        when(!outOfOrder) {
          outOfOrder := True
          goto(INIT)
        } otherwise {
          goto(DONE)
        }
      }

      DONE.whenIsActive(testPass := True)
      ERROR.whenIsActive(testFail := True)
    }

    val blinker = Reg(UInt(30 bits)) init 0
    blinker := blinker + 1

    io.led_n := Mux(testPass, blinker(24), True)
  }
}
