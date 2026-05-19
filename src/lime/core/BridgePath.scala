package lime.core

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._

case class BridgePath() extends Component {
  val io = new Bundle {
    val rx = in(Rgmii.Rx())
    val tx = out(Rgmii.Tx())
    val uart = out(Bool())
  }

  // RX stream
  val rgmiiRx = RgmiiRx()
  rgmiiRx.io.rgmii <> io.rx

  val fifo = new StreamFifoCC(
    dataType = Fragment(Byte),
    depth = 2048,
    pushClock = rgmiiRx.rxClockDomain,
    popClock = ClockDomain.current
  )
  fifo.io.push << rgmiiRx.io.output

  // IGMP stream
  val igmpGen = StaticPacketGen()

  // Arbiter
  val arbiter = StreamArbiterFactory().lowerFirst.fragmentLock.build(Fragment(Byte), 2)
  arbiter.io.inputs(0) <-< fifo.io.pop
  arbiter.io.inputs(1) <-< igmpGen.io.output

  val rgmiiTx = RgmiiTx()
  rgmiiTx.io.rgmii <> io.tx
  rgmiiTx.io.input <-/< arbiter.io.output

  // Network stack
  val eth = EthRx()
  eth.io.input <-< fifo.io.pop.toFlowFire

  val ip = Ipv4Rx()
  ip.io.input <-< eth.io.output.throwWhen(eth.io.header.etherType =/= Ipv4.ETHER_TYPE)

  val udp = UdpRx()
  udp.io.input <-< ip.io.output.throwWhen(ip.io.header.protocol =/= Udp.PROTOCOL_ID)

  val sniffer = FrameSniffer(4096)
  sniffer.io.tap <-< udp.io.output

  val uartTx = UartTx(1_000_000)
  uartTx.io.write << sniffer.io.output
  io.uart := uartTx.io.tx
}
