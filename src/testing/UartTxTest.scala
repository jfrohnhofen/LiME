package lime.testing

import lime.util._
import spinal.core._
import spinal.lib._

class UartTxTest extends Component {
  val io = new Bundle {
    val uart_tx = out Bool ()
  }
  noIoPrefix()

  val uart = new UartTx(baudRate = 1_000_000)
  io.uart_tx := uart.io.tx

  // Send incrementing bytes continuously
  val counter = Reg(UInt(8 bits)) init 0
  uart.io.write.valid := True
  uart.io.write.payload := counter.asBits
  when(uart.io.write.fire) { counter := counter + 1 }
}
