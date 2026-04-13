package lime.testing

import lime.eth._
import spinal.core._
import spinal.lib._

// Transparent Ethernet bridge: forwards every received frame from phy0 to phy1 and vice versa.
//
// Key insight: use phy0's recovered RX clock as phy1's TX clock (and vice versa).
// Both PHYs run at 125 MHz, so the clocks are frequency-matched and no CDC is needed.
//
// RgmiiTx enforces the 12-byte IPG; RgmiiRx will drop an incoming frame if TX is not
// ready (i.e. still in IPG), but in practice minimum-IPG back-to-back frames are rare.
class RgmiiBridgeTest extends Component {
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
    tx1.io.input << rx0.io.output
  }

  // phy1 RX → phy0 TX  (phy1's rxc drives phy0's txc)
  val rx1 = new RgmiiRx
  rx1.io.rgmii <> io.phy1.rx

  new ClockingArea(rx1.rxClockDomain) {
    val tx0 = new RgmiiTx
    tx0.io.rgmii <> io.phy0.tx
    tx0.io.input << rx1.io.output
  }
}
