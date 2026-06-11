package examples

import lime.core._
import lime.output._

abstract class Panel(startUniverse: Int) extends Controller {
  final val numLeds = 150

  for {
    (connector, i) <- io.hub75.connectors.take(4).zipWithIndex
    (pin, j) <- connector.pins.zipWithIndex
  } Ws2812b(pin, startUniverse + i * connector.pins.length + j, numLeds)
}

class PanelA extends Panel(256)
class PanelB extends Panel(512)
class PanelC extends Panel(768)
class PanelD extends Panel(1024)
class PanelE extends Panel(1280)
class PanelF extends Panel(1536)

// Mixed configuration: a WS2812B string whose buffer swap is deferred until a sync
// packet for universe 1 arrives, and a chain of 64 shift-register channels
// (data/clock/latch on the j2 connector).
class Mixed extends Controller {
  Ws2812b(io.hub75.j1.r0, startUniverse = 100, numLeds = 150, syncUniverse = Some(1))
  ShiftRegisterLed(io.hub75.j2.r0, io.hub75.j2.g0, io.hub75.j2.b0, startUniverse = 101, numOutputs = 64)
}
