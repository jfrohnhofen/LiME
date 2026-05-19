package testing

import lime.util._

import spinal.core._
import spinal.lib._

class UartTxTest extends Component {
  val io = new Bundle {
    val uart = out(Bool()).setName("hub75_j16_r0")
  }
  noIoPrefix()

  val uartTx = UartTx(baudRate = 1_000_000)
  io.uart := uartTx.io.tx

  // Send incrementing bytes continuously
  val counter = Reg(UInt(8 bits)) init 0
  uartTx.io.write.valid := True
  uartTx.io.write.payload := counter.asBits
  when(uartTx.io.write.fire) { counter := counter + 1 }
}
