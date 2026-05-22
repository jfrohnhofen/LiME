package lime.output

case class UniverseConfig(id: Int, syncUniverse: Option[Int])

trait Output {
  def universes: Seq[UniverseConfig]
}
