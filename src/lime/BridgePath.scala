package lime.core

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._

class BridgePath extends Component {
  val io = new Bundle {
    val rx = RgmiiRxIo()
    val tx = RgmiiTxIo()
    val uart = out Bool ()
  }

  // RX stream
  val rx = new RgmiiRx
  rx.io.rgmii <> io.rx

  val fifo = new StreamFifoCC(
    dataType = Fragment(Byte),
    depth = 2048,
    pushClock = rx.rxClockDomain,
    popClock = ClockDomain.current
  )
  fifo.io.push << rx.io.output.toStream


  // IGMP stream
  val igmpGen = new StaticPacketGen


  // Arbiter
  val arbiter = StreamArbiterFactory().lowerFirst.fragmentLock.build(Fragment(Byte), 2)
  arbiter.io.inputs(0) <-< fifo.io.pop
  arbiter.io.inputs(1) <-< igmpGen.io.output

  val tx = new RgmiiTx
  tx.io.rgmii <> io.tx
  tx.io.input <-/< arbiter.io.output


  // Network stack
  val eth = new EthRx
  eth.io.input <-< fifo.io.pop.toFlowFire

  val ip = new Ipv4Rx
  ip.io.input <-< eth.io.output.throwWhen(eth.io.header.etherType =/= 0x0800)

  val sniffer = new FrameSniffer(4096)
  sniffer.io.tap <-< ip.io.output

  val uartTx = new UartTx(1_000_000)
  uartTx.io.write << sniffer.io.output
  io.uart := uartTx.io.tx
}
