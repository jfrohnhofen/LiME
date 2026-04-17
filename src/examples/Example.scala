package examples

import lime.core._

class Example extends Controller {
  val mac = Mac("02:00:00:00:00:01")
  val ip = Ip("192.168.1.10")

  val universes = Seq(1, 2, 3)
}
