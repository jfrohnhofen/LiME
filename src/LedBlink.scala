package lime

import spinal.core._
import spinal.lib._

class LedBlink extends Component {
  val io = new Bundle {
    val led_n = out Bool ()
  }
  noIoPrefix()

  val counter = CounterFreeRun(25000000) // 1 Hz at 25 MHz
  val state = Reg(Bool()) init (False)

  when(counter.willOverflow) {
    state := !state
  }

  io.led_n := state
}

object LedBlink extends App { Config.generateVerilog(new LedBlink()) }
