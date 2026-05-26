package testing

import lime.net._
import lime.util._

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

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

  val uartCtrl = UartCtrl(
    UartCtrlInitConfig(
      baudrate = 1_562_500,
      dataLength = 7,
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    )
  )
  uartCtrl.io.uart.rxd := True
  uartCtrl.io.write << fifo.io.pop.map(_.fragment)
  io.uart := uartCtrl.io.uart.txd
}
