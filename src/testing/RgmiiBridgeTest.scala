package testing

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._

// Transparent Ethernet bridge: forwards every received frame from phy0 to phy1 and vice versa.
class RgmiiBridgeTest extends Component {
  val io = new Bundle {
    val phy0 = master(Rgmii())
    val phy1 = master(Rgmii())
  }
  noIoPrefix()

  val clocks = Clocking()
  clocks.io.clk := ClockDomain.current.readClockWire

  // phy0 RX → phy1 TX
  val rx0 = new RgmiiRx()
  rx0.io.rgmii <> io.phy0.rx

  val fifo0 = new StreamFifoCC(
    dataType = Fragment(Byte),
    depth = 128,
    pushClock = rx0.rxClockDomain,
    popClock = clocks.network
  )
  fifo0.io.push << rx0.io.output

  new ClockingArea(clocks.network) {
    val tx1 = RgmiiTx()
    tx1.io.rgmii <> io.phy1.tx
    tx1.io.input << fifo0.io.pop.m2sPipe()
  }

  // phy1 RX → phy0 TX
  val rx1 = RgmiiRx()
  rx1.io.rgmii <> io.phy1.rx

  val fifo1 = new StreamFifoCC(
    dataType = Fragment(Byte),
    depth = 128,
    pushClock = rx1.rxClockDomain,
    popClock = clocks.network
  )
  fifo1.io.push << rx1.io.output

  new ClockingArea(clocks.network) {
    val tx0 = RgmiiTx()
    tx0.io.rgmii <> io.phy0.tx
    tx0.io.input << fifo1.io.pop.m2sPipe()
  }
}
