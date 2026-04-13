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

  // CDC: rxClockDomain → system clock.
  // ready fed back to RgmiiRx: new frames are dropped when the FIFO is full.
  val fifo = new StreamFifoCC(
    dataType  = Fragment(Bits(8 bits)),
    depth     = 512,
    pushClock = rx.rxClockDomain,
    popClock  = ClockDomain.current
  )

  fifo.io.push << rx.io.output

  val uart = new UartTx(baudRate = 1_000_000)
  uart.io.write << fifo.io.pop.map(_.fragment)
  io.uart_tx := uart.io.tx
}
