package lime.core

import lime.net._
import lime.util._
import spinal.core._
import spinal.lib._

class BridgePath extends Component {
  val io = new Bundle {
    val rx = RgmiiRxIo()
    val tx = RgmiiTxIo()
    val uartTx = out Bool ()
  }

  val rx = new RgmiiRx
  rx.io.rgmii <> io.rx

  val fifo = new StreamFifoCC(
    dataType = Fragment(Byte),
    depth = 2048,
    pushClock = rx.rxClockDomain,
    popClock = ClockDomain.current
  )
  fifo.io.push << rx.io.output.toStream

  val packetGen = new StaticPacketGen

  // lowerFirst: input 0 wins whenever valid, so input 1 only starts when FIFO is empty
  // fragmentLock: once input 1 starts a packet it runs to last before re-arbitrating
  val arbiter = StreamArbiterFactory().lowerFirst.fragmentLock.build(Fragment(Byte), 2)
  arbiter.io.inputs(0) << fifo.io.pop.m2sPipe()
  arbiter.io.inputs(1) << packetGen.io.output

  val tx = new RgmiiTx
  tx.io.rgmii <> io.tx
  tx.io.input << arbiter.io.output
}
