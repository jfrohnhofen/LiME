package testing

import lime.net._
import lime.util._

import spinal.core._
import spinal.lib._

class RgmiiRxTest extends Component {
  val io = new Bundle {
    val phy0 = in(Rgmii.Rx())
    val uart = out(Bool()).setName("hub75_j16_r0")
  }
  noIoPrefix()

  val rx = RgmiiRx()
  rx.io.rgmii <> io.phy0

  val fifo = new StreamFifoCC(
    dataType = Fragment(Byte),
    depth = 512,
    pushClock = rx.rxClockDomain,
    popClock = ClockDomain.current
  )
  fifo.io.push << rx.io.output

  val uart = UartTx(baudRate = 1_000_000)
  uart.io.write << fifo.io.pop.map(_.fragment)
  io.uart := uart.io.tx
}
