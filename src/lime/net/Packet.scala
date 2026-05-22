package lime.net

import lime.util._
import spinal.core._
import spinal.lib._

case class Packet() extends Bundle with IMasterSlave {
  val payload = Flow(Fragment(Byte))
  val eth = EthHeader()
  val ip = Ipv4Header()
  val udp = UdpHeader()
  val sacn = SacnHeader()

  override def asMaster(): Unit = {
    master(payload)
    out(eth)
    out(ip)
    out(udp)
    out(sacn)
  }

  def throwWhen(cond: Bool): Packet = {
    val res = cloneOf(this)
    res.payload := this.payload.throwWhen(cond)
    res.eth := this.eth
    res.ip := this.ip
    res.udp := this.udp
    res.sacn := this.sacn
    res
  }

  def stage(): Packet = {
    val res = cloneOf(this)
    res.payload := this.payload.stage()
    res.eth := RegNextWhen(this.eth, this.payload.valid && this.payload.first)
    res.ip := RegNextWhen(this.ip, this.payload.valid && this.payload.first)
    res.udp := RegNextWhen(this.udp, this.payload.valid && this.payload.first)
    res.sacn := RegNextWhen(this.sacn, this.payload.valid && this.payload.first)
    res
  }

  def <-<(that: Packet): Unit = {
    this := that.stage()
  }
}
