package lime.util

import spinal.core._
import spinal.lib._

// Taps a fragment stream without backpressuring upstream.
// Accepts or drops whole frames based on FIFO occupancy at frame start.
class FrameSniffer(fifoDepth: Int = 4096) extends Component {
  val io = new Bundle {
    val tap = slave Flow (Fragment(Byte))
    val output = master Stream (Byte)
  }

  val fifo = StreamFifo(Byte, fifoDepth)
  io.output << fifo.io.pop

  val dropping = RegInit(False)
  val isFirstBeat = RegInit(True)

  fifo.io.push.valid := False
  fifo.io.push.payload := io.tap.payload.fragment

  when(io.tap.valid) {
    when(isFirstBeat) {
      // Drop if FIFO doesn't have room for a max-size Ethernet frame
      dropping := fifo.io.occupancy > (fifoDepth - 1536)
      isFirstBeat := False
    }
    when(!dropping) {
      fifo.io.push.valid := True
      when(!fifo.io.push.ready) { // overflow guard (shouldn't fire with correct threshold)
        dropping := True
      }
    }
    when(io.tap.payload.last) {
      isFirstBeat := True
    }
  }
}
