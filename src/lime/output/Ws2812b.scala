package lime.output

import spinal.core._
import spinal.lib._

case class Ws2812b(
    val startUniverse: Int,
    val numLeds: Int,
    val bytesPerLed: Int,
    val syncUniverse: Option[Int],
    val allowUniverseSpanning: Boolean
) extends Component
    with Output {
  val numUniverses = {
    val numBytes = numLeds * bytesPerLed
    val bytesPerUniverse = if (allowUniverseSpanning) 512 else 512 - (512 % bytesPerLed)
    java.lang.Math.ceilDiv(numBytes, bytesPerUniverse)
  }

  override def universes = (0 until numUniverses).map(i => UniverseConfig(i + startUniverse, syncUniverse))

  val io = new Bundle {
    val pin = out Bool ()
  }

  io.pin := Bool(startUniverse % 2 == 1)
}

object Ws2812b {
  def apply(
      pin: Bool,
      startUniverse: Int,
      numLeds: Int,
      bytesPerLed: Int = 3,
      syncUniverse: Option[Int] = None,
      allowUniverseSpanning: Boolean = false
  ): Ws2812b = {
    val ws = Ws2812b(startUniverse, numLeds, bytesPerLed, syncUniverse, allowUniverseSpanning)
    pin := ws.io.pin
    ws
  }
}
