package lime.testing

import lime.eth._
import spinal.core._
import spinal.lib._

// Transparent Ethernet bridge: forwards every received byte from phy0 to phy1 and vice versa.
//
// Key insight: use phy0's recovered RX clock as phy1's TX clock (and vice versa).
// Both PHYs run at 125 MHz, so the clocks are frequency-matched and no CDC is needed.
// Forwarding is purely combinatorial — latency is just a few clock cycles (~24 ns).
class EthernetBridgeTest extends Component {
  val io = new Bundle {
    val phy0 = RgmiiIo()
    val phy1 = RgmiiIo()
  }
  noIoPrefix()

  // phy0 RX → phy1 TX  (phy0's rxc drives phy1's txc)
  val rx0 = new RgmiiRx
  rx0.io.rgmii <> io.phy0.rx

  new ClockingArea(rx0.rxClockDomain) {
    val tx1 = new RgmiiTx
    tx1.io.rgmii <> io.phy1.tx

    tx1.io.input.valid := rx0.io.output.valid
    tx1.io.input.payload := rx0.io.output.payload
  }

  // phy1 RX → phy0 TX  (phy1's rxc drives phy0's txc)
  val rx1 = new RgmiiRx
  rx1.io.rgmii <> io.phy1.rx

  new ClockingArea(rx1.rxClockDomain) {
    val tx0 = new RgmiiTx
    tx0.io.rgmii <> io.phy0.tx

    tx0.io.input.valid := rx1.io.output.valid
    tx0.io.input.payload := rx1.io.output.payload
  }
}
