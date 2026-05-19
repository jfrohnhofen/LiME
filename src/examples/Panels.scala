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
