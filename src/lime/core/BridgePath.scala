package lime.core

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

case class BridgePath() extends Component {
  val io = new Bundle {
    val rx = in(Rgmii.Rx())
    val tx = out(Rgmii.Tx())
    val uart = out(Bool())

    // val sacnCmd = master(Flow(WriteCmd()))
    // val sacnSync = out Bool ()
    // val sacnSyncAddr = out UInt (16 bits)
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
  ip.io.input <-< eth.io.output.throwWhen(eth.io.output.eth.etherType =/= Ipv4.ETHER_TYPE)

  val udp = UdpRx()
  udp.io.input <-< ip.io.output.throwWhen(ip.io.output.ip.protocol =/= Udp.PROTOCOL_ID)

  val sacn = SacnRx()
  sacn.io.input <-< udp.io.output.throwWhen(udp.io.output.udp.dstPort =/= Sacn.PORT)

  val sniffer = FrameSniffer(4096)
  sniffer.io.tap <-< sacn.io.output.payload

  val uartCtrl = UartCtrl(
    UartCtrlInitConfig(
      baudrate = 1_562_500,
      dataLength = 7,
      parity = UartParityType.NONE,
      stop = UartStopType.ONE
    )
  )
  uartCtrl.io.uart.rxd := True
  uartCtrl.io.write <-< sniffer.io.output
  io.uart := uartCtrl.io.uart.txd

//  val writer = Writer(pathId)
//  writer.io.input.valid := sacn.io.output.valid
//  writer.io.input.payload.fragment := sacn.io.output.fragment
//  writer.io.input.payload.last := sacn.io.output.last
//  writer.io.header := sacn.io.header
//  io.sacnCmd <> writer.io.cmd

//  io.sacnSync := sacn.io.sync
//  io.sacnSyncAddr := sacn.io.header.syncAddress

}
