package lime

import spinal.core._
import spinal.lib._
import javax.net.ssl.TrustManager

class IDDRX1F extends BlackBox {
  val io = new Bundle {
    val D = in Bool ()
    val SCLK = in Bool ()
    val RST = in Bool ()
    val Q0 = out Bool ()
    val Q1 = out Bool ()
  }

  mapClockDomain(clock = io.SCLK /*, reset=io.RST*/ )
  noIoPrefix()
}

class DELAYG(mode: String, value: Int) extends BlackBox {
  addGeneric("DEL_MODE", mode)
  addGeneric("DEL_VALUE", value)

  val io = new Bundle {
    val A = in Bool ()
    val Z = out Bool ()
  }

  noIoPrefix()
}

case class Rgmii() extends Bundle with IMasterSlave {
  val clk = Bool()
  val data = Bits(4 bits)
  val ctl = Bool()

  override def asMaster(): Unit = {
    out(clk, ctl, data)
  }
}

class RgmiiRx() extends Component {
  val io = new Bundle {
    val rgmii = slave(Rgmii())
    val output = master(Flow(Bits(8 bits)))
  }

  val rxClockDomain = ClockDomain(
    clock = io.rgmii.clk
  )

  new ClockingArea(rxClockDomain) {
    val ctlDelay = new DELAYG("USER_DEFINED", 80)
    val ctlIddr = new IDDRX1F()

    ctlDelay.io.A := io.rgmii.ctl
    ctlIddr.io.D := ctlDelay.io.Z
    ctlIddr.io.RST := False
    io.output.valid := ctlIddr.io.Q0 && ctlIddr.io.Q1

    val dataDelay = for (i <- 0 until 4) yield new DELAYG("USER_DEFINED", 80)
    val dataIddr = for (i <- 0 until 4) yield new IDDRX1F()

    for (i <- 0 until 4) {
      dataDelay(i).io.A := io.rgmii.data(i)
      dataIddr(i).io.D := dataDelay(i).io.Z
      dataIddr(i).io.RST := False
      io.output.payload(i) := dataIddr(i).io.Q0
      io.output.payload(i + 4) := dataIddr(i).io.Q1
    }
  }
}

class Sacn extends Component {
  val io = new Bundle {
    val input = slave(Flow(Bits(8 bits)))
    val output = master(Flow(Bits(8 bits)))
  }

  object State extends SpinalEnum {
    val PREAMBLE, DATA = newElement()
  }
  import State._

  val state = RegInit(PREAMBLE)
  val counter = CounterFreeRun(512)
  io.output := io.input.throwWhen(state !== DATA)

  /*switch (state) {
    is(PREAMBLE) {
      counter.clear()
      when (io.input.valid && io.input.payload === 0x55) {
        counter.increment()
        when (counter.value === 6) {
          state := DATA
        }
      }
    }

    is(DATA) {
      when(!io.input.valid) {
        state := PREAMBLE
      }
    }
  }*/
}

class RgmiiTest extends Component {
  val io = new Bundle {
    val phy0_rgmii_rx = slave(Rgmii())
    val uart_tx = out Bool ()
  }
  noIoPrefix()

  val rgmii_rx = new RgmiiRx()
  rgmii_rx.io.rgmii := io.phy0_rgmii_rx


  val fifo = StreamFifoCC(
    dataType = Bits(8 bits),
    depth = 2048,
    pushClock = rgmii_rx.rxClockDomain,
    popClock = ClockDomain.current
  )


  new ClockingArea(rgmii_rx.rxClockDomain) {
    val sacn = new Sacn()
    sacn.io.input <> rgmii_rx.io.output
    fifo.io.push << sacn.io.output.toStream
  }

  val uart = new UartTx(baudRate = 115200, dataLength = 8)
  io.uart_tx := uart.io.tx

  uart.io.write << fifo.io.pop
}

object RgmiiTest extends App { Config.generateVerilog(new RgmiiTest()) }
