package lime

import spinal.core._
import spinal.lib._
import spinal.lib.com.uart._

class UartTest extends Component {
  val io = new Bundle {
    val uart_tx = out Bool ()
  }
  noIoPrefix()

  val uart = new UartTx(baudRate = 115200, dataLength = 8)
  io.uart_tx := uart.io.tx

  val mem = new MemToStream()
  uart.io.write <> mem.io.output

  val timer = CounterFreeRun(ClockDomain.current.frequency.getValue.toInt)
  mem.io.start := timer.willOverflow
}

class MemToStream extends Component {
  val io = new Bundle {
    val start = in Bool ()
    val output = master Stream (Bits(8 bits))
  }

  val mem = Mem(Bits(8 bits), wordCount = 2048)
  mem.init(Seq.tabulate(2048)(i => i % 256))

  val address = Reg(UInt(log2Up(mem.wordCount) bits)) init (0)
  val active = Reg(Bool()) init (False)

  when(io.start && !active) {
    active := True
    address := 0
  }

  val memData = mem.readSync(address)

  io.output.valid := active
  io.output.payload := memData

  when(io.output.fire) {
    address := address + 1
    when(address === mem.wordCount - 1) {
      active := False
    }
  }
}

object UartTest extends App { Config.generateVerilog(new UartTest()) }
