package lime

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
    )
