package lime.util

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

case class UartTx(baudRate: Int) extends Component {
  val io = new Bundle {
    val tx = out(Bool())
    val write = slave(Stream(Byte))
  }

  val ticksPerBit = (ClockDomain.current.frequency.getValue / baudRate).toInt
  val counter = CounterFreeRun(ticksPerBit)
  val bitIndex = Reg(UInt(3 bits))
  val payload = Reg(Byte)

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val START = new State
    val DATA = new State
    val STOP = new State

    IDLE.whenIsActive {
      when(io.write.fire) {
        payload := io.write.payload
        counter.clear()
        goto(START)
      }
    }

    START.whenIsActive {
      when(counter.willOverflow) {
        counter.clear()
        bitIndex := 0
        goto(DATA)
      }
    }

    DATA.whenIsActive {
      when(counter.willOverflow) {
        counter.clear()
        when(bitIndex === 7) { goto(STOP) }
        bitIndex := bitIndex + 1
      }
    }

    STOP.whenIsActive {
      when(counter.willOverflow) { goto(IDLE) }
    }
  }

  io.write.ready := fsm.isActive(fsm.IDLE)
  io.tx := !fsm.isActive(fsm.START) && (fsm.isActive(fsm.DATA) ? payload(bitIndex) | True)
}
