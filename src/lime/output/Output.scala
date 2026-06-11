package lime.output

import lime.core.SdramReadPort
import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class UniverseConfig(id: Int, syncUniverse: Option[Int] = None, doubleBuffered: Boolean = true)

// An LED output. Instantiate via the companion apply of a concrete implementation from
// directly inside a Controller subclass; the Controller picks up all outputs, allocates
// their SDRAM slots and wires read/sel at the end of elaboration.
trait Output { this: Component =>
  def baseIndex: Int // first SDRAM universe slot, allocated by the Controller
  def universes: Seq[UniverseConfig]
  def read: SdramReadPort
  def sel: Vec[Bool] // front-buffer select per universe, driven by the UniverseStore
}

object Output {
  // How the channels of one output spread over 512-byte universes: either packed densely
  // (spanning, units may straddle a universe boundary) or whole units per universe.
  def universeBytes(totalBytes: Int, bytesPerUnit: Int, spanning: Boolean): Seq[Int] = {
    require(totalBytes > 0)
    val per = if (spanning) 512 else 512 - 512 % bytesPerUnit
    require(per > 0, s"bytesPerUnit $bytesPerUnit too large for one universe")
    (0 until totalBytes by per).map(off => (totalBytes - off) min per)
  }
}

object Gamma {
  // 8-bit input to (8 + ditherBits)-bit fixed-point gamma curve, full scale 255 << ditherBits
  def table(gamma: Double, ditherBits: Int): Seq[Int] =
    (0 until 256).map(i => scala.math.round(scala.math.pow(i / 255.0, gamma) * (255 << ditherBits)).toInt)
}

// Continuously streams the bytes of one output's universes from SDRAM, frame after frame,
// in DMX channel order. The last byte of each frame is marked with fragment.last.
// Issues one outstanding word read at a time; downstream backpressure paces the loop.
case class UniverseFetcher(baseIndex: Int, uBytes: Seq[Int]) extends Component {
  val numU = uBytes.length
  require(numU > 0 && uBytes.forall(b => b > 0 && b <= 512))

  val io = new Bundle {
    val read = master(SdramReadPort())
    val sel = in Vec (Bool(), numU)
    val bytes = master Stream (Fragment(Byte))
  }

  val fifo = StreamFifo(Fragment(Byte), 16)
  io.bytes << fifo.io.pop

  val uWidth = log2Up(numU)
  val u = Reg(UInt(uWidth bits)) init 0
  val w = Reg(UInt(7 bits)) init 0
  val lane = Reg(UInt(2 bits)) init 0
  val rspData = Reg(Bits(32 bits))

  val uLastByte = Vec(uBytes.map(b => U(b - 1, 9 bits)))
  val byteOffset = (w ## lane).asUInt

  val frameLast = byteOffset === uLastByte(u) && u === numU - 1

  fifo.io.push.valid := False
  fifo.io.push.fragment := rspData.byte(lane)
  fifo.io.push.last := frameLast

  io.read.cmd.valid := False
  io.read.cmd.payload := ((U(baseIndex, 13 bits) + u) @@ io.sel(u) @@ w).resized

  val fsm = new StateMachine {
    val REQ = new State with EntryPoint
    val WAIT = new State
    val UNPACK = new State

    REQ.whenIsActive {
      io.read.cmd.valid := True
      when(io.read.cmd.ready) { goto(WAIT) }
    }

    WAIT.whenIsActive {
      when(io.read.rsp.valid) {
        rspData := io.read.rsp.payload
        lane := 0
        goto(UNPACK)
      }
    }

    UNPACK.whenIsActive {
      fifo.io.push.valid := True
      when(fifo.io.push.ready) {
        when(byteOffset === uLastByte(u)) {
          lane := 0
          w := 0
          u := Mux(u === numU - 1, U(0, uWidth bits), (u + 1).resize(uWidth))
          goto(REQ)
        } elsewhen (lane === 3) {
          lane := 0
          w := w + 1
          goto(REQ)
        } otherwise {
          lane := lane + 1
        }
      }
    }
  }
}
