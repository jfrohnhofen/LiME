package testing

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._

class RgmiiTxTest extends Component {
  val io = new Bundle {
    val phy0 = out(Rgmii.Tx())
  }
  noIoPrefix()

  // PLL: 25 MHz system clock → 125 MHz TX clock
  val pll = Pll25to125()
  pll.io.CLKI := ClockDomain.current.readClockWire
  pll.io.CLKFB := pll.io.CLKOP // internal feedback
  pll.io.RST := False
  pll.io.STDBY := False

  val frameBytes: Seq[Int] = Seq(
    // Destination MAC (Broadcast)
    0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
    // Source MAC
    0x00, 0x0a, 0x35, 0x00, 0x01, 0x22,
    // EtherType (ARP)
    0x08, 0x06,
    // Hardware Type (Ethernet = 1) & Protocol Type (IPv4 = 0x0800)
    0x00, 0x01, 0x08, 0x00,
    // Hardware Size (6) & Protocol Size (4) & Opcode (Request = 1)
    0x06, 0x04, 0x00, 0x01,
    // Sender MAC Address
    0x00, 0x0a, 0x35, 0x00, 0x01, 0x22,
    // Sender IP Address (192.168.1.100)
    0xc0, 0xa8, 0x01, 0x64,
    // Target MAC Address (Ignored in Request)
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    // Target IP Address (192.168.1.1)
    0xc0, 0xa8, 0x01, 0x01,
    // Padding (to reach minimum 60-byte Ethernet payload size)
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    // Frame Check Sequence (FCS / CRC-32)
    0x9b, 0xf8, 0x2c, 0x5b
  )

  val txClockDomain = ClockDomain(
    clock = pll.io.CLKOP,
    config = ClockDomainConfig(resetKind = BOOT)
  )

  new ClockingArea(txClockDomain) {
    val tx = new RgmiiTx()
    tx.io.rgmii <> io.phy0

    val frameLen = frameBytes.length
    val rom = Vec(frameBytes.map(b => B(b, 8 bits)))

    val sending = Reg(Bool()) init False
    val idx = Reg(UInt(log2Up(frameLen) bits)) init 0
    val gap = Reg(UInt(20 bits)) init 0

    when(sending) {
      when(tx.io.input.fire) {
        when(idx === frameLen - 1) {
          sending := False
          idx := 0
          gap := 999999 // ~8 ms gap at 125 MHz before next frame
        } otherwise {
          idx := idx + 1
        }
      }
    } otherwise {
      when(gap === 0) {
        sending := True
      } otherwise {
        gap := gap - 1
      }
    }

    tx.io.input.valid := sending
    tx.io.input.payload.fragment := rom(idx)
    tx.io.input.payload.last := idx === frameLen - 1
  }
}
