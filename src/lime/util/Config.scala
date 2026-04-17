package lime.util

import spinal.core._

object Config
    extends SpinalConfig(
      device = Device.LATTICE,
      defaultConfigForClockDomains = ClockDomainConfig(
        clockEdge = RISING,
        resetKind = BOOT,
        resetActiveLevel = LOW
      ),
      defaultClockDomainFrequency = FixedFrequency(25 MHz)
    ) {

  def main(args: Array[String]): Unit = {
    val clazz = Class.forName(args(0))
    generateVerilog(clazz.getDeclaredConstructor().newInstance().asInstanceOf[Component])
  }
}
