package lime

import spinal.core._
import spinal.lib._

class UartTx(baudRate: Int, dataLength: Int) extends Component {
  object State extends SpinalEnum {
    val IDLE, START, DATA, STOP = newElement()
  }
  import State._

  val io = new Bundle {
    val tx = out Bool ()
    val write = slave Stream (Bits(dataLength bit))
  }

  val ticksPerBit = (ClockDomain.current.frequency.getValue / baudRate).toInt
  val counter = CounterFreeRun(ticksPerBit)
  val stopCounter = CounterFreeRun(16*ticksPerBit)
  val bitIndex = Reg(UInt(log2Up(dataLength) bits))
  val payload = Reg(Bits(dataLength bit))

  val state = RegInit(IDLE)
  io.write.ready := state === IDLE

  switch(state) {
    is(IDLE) {
      io.tx := True
      when(io.write.fire) {
        payload := io.write.payload
        state := START
        counter.clear()
      }
    }

    is(START) {
      io.tx := False
      when(counter.willOverflow) {
        state := DATA
        counter.clear()
        bitIndex := 0
      }
    }

    is(DATA) {
      io.tx := payload(bitIndex)
      when(counter.willOverflow) {
        when(bitIndex === dataLength - 1) {
          stopCounter.clear()
          state := STOP
        }
        bitIndex := bitIndex + 1
        counter.clear()
      }
    }

    is(STOP) {
      io.tx := True
      when(stopCounter.willOverflow) {
        state := IDLE
      }
    }
  }
}
