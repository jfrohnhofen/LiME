package lime.testing

import lime.eth._
import lime.util._
import spinal.core._
import spinal.lib._

class RgmiiRxTest extends Component {
  val io = new Bundle {
    val phy0_rx = RgmiiRxIo()
    val uart_tx = out Bool ()
  }
  noIoPrefix()

  val rx = new RgmiiRx
  rx.io.rgmii <> io.phy0_rx

  // CDC: rxClockDomain → system clock, drops frames silently when full
  val fifo = new StreamFifoCC(
    dataType = Bits(8 bits),
    depth = 512,
    pushClock = rx.rxClockDomain,
    popClock = ClockDomain.current
  )

  new ClockingArea(rx.rxClockDomain) {
    fifo.io.push.valid := rx.io.output.valid
    fifo.io.push.payload := rx.io.output.payload
  }

  val uart = new UartTx(baudRate = 1_000_000, dataLength = 8)
  uart.io.write << fifo.io.pop
  io.uart_tx := uart.io.tx
}
