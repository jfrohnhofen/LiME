package lime.output

import lime.core.{Controller, SdramReadPort}
import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

// WS2812B string driver.
//
// Streams the universe data from SDRAM frame after frame and serializes it at 800 kHz
// (1.25 us/bit, 0.4/0.8 us high, 300 us latch gap between frames).
//
// Gamma correction uses a block-RAM lookup table with (8 + ditherBits) bits of output
// resolution; the fractional bits are recovered by temporal dithering: every refresh the
// LUT value is offset by a bit-reversed frame phase before truncation, so a channel value
// between two 8-bit steps toggles between them at the refresh rate and averages out.
// This only works (invisibly) if the string refreshes fast enough, hence the elaboration
// warning driven by minRefreshHz.
//
// DMX channels map 1:1 onto the WS2812B byte stream (for RGB strips channel order is
// G,R,B as seen by the LED - lay out the source data accordingly).
case class Ws2812b(
    baseIndex: Int,
    startUniverse: Int,
    numLeds: Int,
    bytesPerLed: Int,
    syncUniverse: Option[Int],
    allowUniverseSpanning: Boolean,
    doubleBuffered: Boolean,
    gamma: Double,
    ditherBits: Int,
    minRefreshHz: Double
) extends Component
    with Output {
  require(numLeds > 0 && bytesPerLed > 0 && ditherBits >= 0 && ditherBits <= 8)

  val uBytes = Output.universeBytes(numLeds * bytesPerLed, bytesPerLed, allowUniverseSpanning)

  override def universes = uBytes.indices.map(i => UniverseConfig(startUniverse + i, syncUniverse, doubleBuffered))

  val fHz = ClockDomain.current.frequency.getValue.toDouble
  val cyclesPerBit = (fHz * 1.25e-6).round.toInt
  val highShort = (fHz * 0.4e-6).round.toInt
  val highLong = (fHz * 0.8e-6).round.toInt
  val resetCycles = (fHz * 300e-6).round.toInt

  val refreshHz = 1.0 / (numLeds * bytesPerLed * 8 * 1.25e-6 + 300e-6)
  if (refreshHz < minRefreshHz) {
    SpinalWarning(
      f"Ws2812b(startUniverse=$startUniverse): $numLeds LEDs refresh at only $refreshHz%.0f Hz " +
        f"(< $minRefreshHz%.0f Hz); temporal dithering may flicker visibly. " +
        f"Use fewer LEDs per pin or lower ditherBits."
    )
  }

  val io = new Bundle {
    val pin = out Bool ()
    val read = master(SdramReadPort())
    val sel = in Vec (Bool(), uBytes.length)
  }

  override def read = io.read
  override def sel = io.sel

  val fetcher = UniverseFetcher(baseIndex, uBytes)
  fetcher.io.read <> io.read
  fetcher.io.sel := io.sel

  val D = ditherBits
  val lut = Mem(UInt((8 + D) bits), 256)
  lut.init(Gamma.table(gamma, D).map(U(_, (8 + D) bits)))
  lut.addAttribute("ram_style", "block")

  val frameCnt = Reg(UInt((D max 1) bits)) init 0
  val phase = if (D > 0) Reverse(frameCnt.asBits.resize(D)).asUInt else U(0, 1 bits)

  val bytes = fetcher.io.bytes
  // Extra register after the BRAM: its ~5 ns clock-to-out plus the dither adder does not
  // fit one cycle
  val lutVal = RegNext(lut.readSync(bytes.fragment.asUInt, bytes.fire))
  val isLast = RegNextWhen(bytes.last, bytes.fire) init False

  val shift = Reg(Bits(8 bits))
  val bitCnt = Reg(UInt(3 bits))
  val cycleCnt = Reg(UInt(log2Up(resetCycles + 1) bits)) init 0
  val pin = RegInit(False)
  io.pin := pin

  val fsm = new StateMachine {
    val FETCH = new State with EntryPoint
    val LUT = new State
    val APPLY = new State
    val SHIFT = new State
    val GAP = new State

    bytes.ready := isActive(FETCH)

    FETCH.whenIsActive {
      when(bytes.valid) { goto(LUT) }
    }

    LUT.whenIsActive { goto(APPLY) } // lutVal register captures the BRAM output

    APPLY.whenIsActive {
      val corrected =
        if (D > 0) ((lutVal + phase.resize(8 + D))(8 + D - 1 downto D)) else lutVal
      shift := corrected.asBits
      bitCnt := 7
      cycleCnt := 0
      goto(SHIFT)
    }

    SHIFT.whenIsActive {
      pin := cycleCnt < Mux(shift(7), U(highLong), U(highShort))
      cycleCnt := cycleCnt + 1
      when(cycleCnt === cyclesPerBit - 1) {
        cycleCnt := 0
        when(bitCnt === 0) {
          when(isLast) {
            frameCnt := frameCnt + 1
            goto(GAP)
          } otherwise {
            goto(FETCH)
          }
        } otherwise {
          bitCnt := bitCnt - 1
          shift := shift |<< 1
        }
      }
    }

    GAP.whenIsActive {
      pin := False
      cycleCnt := cycleCnt + 1
      when(cycleCnt === resetCycles - 1) {
        cycleCnt := 0
        goto(FETCH)
      }
    }
  }
}

object Ws2812b {
  def apply(
      pin: Bool,
      startUniverse: Int,
      numLeds: Int,
      bytesPerLed: Int = 3,
      syncUniverse: Option[Int] = None,
      allowUniverseSpanning: Boolean = false,
      doubleBuffered: Boolean = true,
      gamma: Double = 2.2,
      ditherBits: Int = 3,
      minRefreshHz: Double = 200
  ): Ws2812b = {
    val ctrl = Controller.current
    val numUniverses = Output.universeBytes(numLeds * bytesPerLed, bytesPerLed, allowUniverseSpanning).length
    val base = ctrl.allocateUniverses(numUniverses)
    val area = new ClockingArea(ctrl.systemDomain) {
      val c = Ws2812b(
        base,
        startUniverse,
        numLeds,
        bytesPerLed,
        syncUniverse,
        allowUniverseSpanning,
        doubleBuffered,
        gamma,
        ditherBits,
        minRefreshHz
      )
    }
    pin := area.c.io.pin
    area.c
  }
}
