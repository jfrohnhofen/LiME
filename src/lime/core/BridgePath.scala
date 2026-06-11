package lime.core

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

// One direction of the two-port bridge: forwards every frame from rx to tx (so devices
// can be chained), injects locally generated frames (IGMP reports), and taps the byte
// stream into the network stack to extract sACN data/sync packets as a SacnWrite stream.
//
// debugUart dumps the received sACN payload bytes on io.uart; it costs a 4 KB FIFO and
// makes 125 MHz timing closure harder, so it is off by default.
case class BridgePath(debugUart: Boolean = false) extends Component {
  val io = new Bundle {
    val rx = in(Rgmii.Rx())
    val tx = out(Rgmii.Tx())
    val uart = out(Bool())

    // Locally originated frames (pre-computed, FCS included) merged into the bridge
    val igmp = slave Stream (Fragment(Byte))

    // Parsed sACN traffic towards the UniverseStore
    val write = master Flow (Fragment(SacnWrite()))
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

  // Arbiter: bridged traffic has priority over injected frames
  val arbiter = StreamArbiterFactory().lowerFirst.fragmentLock.build(Fragment(Byte), 2)
  arbiter.io.inputs(0) <-< fifo.io.pop
  arbiter.io.inputs(1) <-< io.igmp

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
  sacn.io.input <-< udp.io.output.throwWhen(
    udp.io.output.udp.dstPort =/= Sacn.PORT ||
      udp.io.output.eth.dstMac.takeHigh(Sacn.MAC_PREFIX.getWidth) =/= Sacn.MAC_PREFIX ||
      udp.io.output.ip.dstIp.takeHigh(Sacn.IP_PREFIX.getWidth) =/= Sacn.IP_PREFIX
  )

  // SacnWrite stream: data beats while a data packet's DMX payload streams, a single
  // sync beat one cycle after a sync packet ends (the two can never collide because sync
  // packets carry no DMX payload and packets are separated by at least the next headers).
  val writer = new Area {
    val payload = sacn.io.output.payload
    val first = RegInit(True)
    when(payload.valid) { first := payload.last }

    // payload.valid and sacn.io.sync are mutually exclusive (sync fires one cycle after
    // a sync packet's last byte, with no DMX payload in flight), so the registered sync
    // pulse can drive all the muxes instead of the deeper payload.valid cone.
    val sync = sacn.io.sync
    val flow = Flow(Fragment(SacnWrite()))
    flow.valid := payload.valid || sync
    flow.fragment.start := first || sync
    flow.fragment.sync := sync
    flow.fragment.complete := sacn.io.complete
    flow.fragment.universe := Mux(sync, sacn.io.output.sacn.syncAddress, sacn.io.output.sacn.universe)
    flow.fragment.priority := sacn.io.output.sacn.priority
    flow.fragment.data := payload.fragment
    flow.last := payload.last || sync

    io.write << flow.stage()
  }

  val debug = if (debugUart) new Area {
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
  }
  else
    new Area {
      io.uart := True // UART idle
    }
}
