package lime

import spinal.core._

// Hardware definition
case class Blink() extends Component {
  noIoPrefix()

  val io = new Bundle {
    val led_n = out Bool ()
  }

  clockDomain.clock.setName("clk")
  clockDomain.reset.setName("rst_n")

  val counter = Reg(UInt(24 bits)) init(0)
  counter := counter + 1
  io.led_n := counter(23)
}

object Blink extends App {
  SpinalConfig(
    defaultConfigForClockDomains = ClockDomainConfig(
      resetActiveLevel = LOW
    ),
  ).generateVerilog(Blink())
}
