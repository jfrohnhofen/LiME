package testing

import lime.util._

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

class UartTxTest extends Component {
  val io = new Bundle {
    val uart = out(Bool()).setName("hub75_j16_r0")
  }
  noIoPrefix()

  val uartCtrl = UartCtrl(uartConfig)
  uartCtrl.io.uart.rxd := True
  io.uart := uartCtrl.io.uart.txd

  // Send incrementing bytes continuously
  val counter = Reg(UInt(8 bits)) init 0
  uartCtrl.io.write.valid := True
  uartCtrl.io.write.payload := counter.asBits
  when(uartCtrl.io.write.fire) { counter := counter + 1 }
}
