package lime.net

import lime.util._
import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object Rgmii {
  case class Rx() extends Bundle {
    val rxc = Bool()
    val rxd = Bits(4 bits)
    val rxctl = Bool()
  }
  case class Tx() extends Bundle {
    val txc = Bool()
    val txd = Bits(4 bits)
    val txctl = Bool()
  }

  final val IPG_BYTES = 12
  final val PREAMBLE_SFD_BYTES = 8
  final val PREAMBLE_VALUE = 0x55
  final val SFD_VALUE = 0xd5
}

case class Rgmii() extends Bundle with IMasterSlave {
  val rxc = Bool()
  val rxd = Bits(4 bits)
  val rxctl = Bool()

  val txc = Bool()
  val txd = Bits(4 bits)
  val txctl = Bool()

  override def asMaster(): Unit = {
    in(rxc, rxd, rxctl)
    out(txc, txd, txctl)
  }

  def rx: Rgmii.Rx = { val rx = Rgmii.Rx(); rx.rxc := rxc; rx.rxd := rxd; rx.rxctl := rxctl; rx }
  def tx: Rgmii.Tx = { val tx = Rgmii.Tx(); txc := tx.txc; txd := tx.txd; txctl := tx.txctl; tx }
}

case class RgmiiRx() extends Component {
  val io = new Bundle {
    val rgmii = in(Rgmii.Rx())
    val output = master(Stream(Fragment(Byte)))
  }

  val rxClockDomain = ClockDomain(
    clock = io.rgmii.rxc,
    config = ClockDomainConfig(resetKind = BOOT)
  )

  new ClockingArea(rxClockDomain) {
    def ddrIn(d: Bool): (Bool, Bool) = {
      val delay = DELAYG("SCLK_ALIGNED", 80)
      val iddr = IDDRX1F()
      delay.io.A := d
      iddr.io.D := delay.io.Z
      iddr.io.RST := False
      (iddr.io.Q0, iddr.io.Q1)
    }

    val lowNibble = Bits(4 bits)
    val highNibble = Bits(4 bits)
    for (i <- 0 until 4) {
      val (rxdRise, rxdFall) = ddrIn(io.rgmii.rxd(i))
      lowNibble(i) := rxdRise
      highNibble(i) := rxdFall
    }
    val rxData = highNibble ## lowNibble
    val rxDataReg = RegNext(rxData) init (0)

    val (rxctlRise, rxctlFall) = ddrIn(io.rgmii.rxctl)
    val rxDv = rxctlRise
    val rxEr = rxctlRise ^ rxctlFall

    val phyIdle = !rxDv && !rxEr
    val phyValid = rxDv && !rxEr
    val phyFalseCarrier = !rxDv && rxEr
    val phyError = rxDv && rxEr

    val committed = Reg(Bool()) init (False)
    val lastByte = Reg(Byte) init (0)

    val fsm = new StateMachine {
      val IDLE = new State with EntryPoint
      val PREAMBLE = new State
      val INFRAME = new State
      val DISCARD_LAST = new State
      val DISCARD = new State

      IDLE.whenIsActive {
        committed := False
        when(phyValid) { goto(PREAMBLE) }
          .elsewhen(phyFalseCarrier || phyError) { goto(DISCARD) }
      }

      PREAMBLE.whenIsActive {
        when(phyIdle) { goto(IDLE) }
          .elsewhen(phyFalseCarrier || phyError) { goto(DISCARD) }
          .elsewhen(rxDataReg === Rgmii.SFD_VALUE) { goto(INFRAME) }
      }

      INFRAME.whenIsActive {
        when(io.output.fire) {
          committed := phyValid
          when(phyIdle) { goto(IDLE) }
            .elsewhen(phyFalseCarrier || phyError) { goto(DISCARD) }
        }.otherwise {
          when(committed) {
            lastByte := rxDataReg
            goto(DISCARD_LAST)
          }.otherwise {
            goto(DISCARD)
          }
        }
      }

      DISCARD_LAST.whenIsActive {
        when(io.output.fire) {
          when(phyIdle) { goto(IDLE) }
            .otherwise { goto(DISCARD) }
        }
      }

      DISCARD.whenIsActive {
        when(phyIdle) { goto(IDLE) }
      }
    }

    io.output.valid := fsm.isActive(fsm.INFRAME) || fsm.isActive(fsm.DISCARD_LAST)
    io.output.fragment := Mux(fsm.isActive(fsm.DISCARD_LAST), lastByte, rxDataReg)
    io.output.last := fsm.isActive(fsm.DISCARD_LAST) || (fsm.isActive(fsm.INFRAME) && !phyValid)
  }
}

case class RgmiiTx() extends Component {
  val io = new Bundle {
    val rgmii = out(Rgmii.Tx())
    val input = slave(Stream(Fragment(Byte)))
  }

  def ddrOut(rising: Bool, falling: Bool): Bool = {
    val oddr = ODDRX1F()
    oddr.io.D0 := rising
    oddr.io.D1 := falling
    oddr.io.RST := False
    oddr.io.Q
  }

  val ipgCounter = Reg(UInt(log2Up(Rgmii.IPG_BYTES) bits)) init (Rgmii.IPG_BYTES - 1)
  val preambleCounter = Reg(UInt(log2Up(Rgmii.PREAMBLE_SFD_BYTES) bits)) init (Rgmii.PREAMBLE_SFD_BYTES - 1)

  val txDv = Reg(Bool()) init (False)
  val txEr = Reg(Bool()) init (False)
  val txData = Reg(Bits(8 bits)) init (0)

  val fsm = new StateMachine {
    val IDLE = new State with EntryPoint
    val PREAMBLE = new State
    val DATA = new State
    val FLUSH = new State
    val IPG = new State

    IDLE.whenIsActive {
      txDv := False
      txEr := False
      when(io.input.valid) {
        preambleCounter := Rgmii.PREAMBLE_SFD_BYTES - 1
        goto(PREAMBLE)
      }
    }

    PREAMBLE.whenIsActive {
      txDv := True
      txEr := False
      txData := Mux(preambleCounter === 0, B(Rgmii.SFD_VALUE, 8 bits), B(Rgmii.PREAMBLE_VALUE, 8 bits))

      preambleCounter := preambleCounter - 1
      when(preambleCounter === 0) { goto(DATA) }
    }

    DATA.whenIsActive {
      txDv := True
      txEr := False
      txData := io.input.fragment

      when(io.input.fire) {
        when(io.input.last) {
          ipgCounter := Rgmii.IPG_BYTES - 1
          goto(IPG)
        }
      }.otherwise {
        txEr := True
        goto(FLUSH)
      }
    }

    FLUSH.whenIsActive {
      txDv := False
      txEr := False

      when(io.input.fire && io.input.last) {
        ipgCounter := Rgmii.IPG_BYTES - 1
        goto(IPG)
      }
    }

    IPG.whenIsActive {
      txDv := False
      txEr := False

      ipgCounter := ipgCounter - 1
      when(ipgCounter === 0) {
        when(io.input.valid) {
          preambleCounter := Rgmii.PREAMBLE_SFD_BYTES - 1
          goto(PREAMBLE)
        } otherwise {
          goto(IDLE)
        }
      }
    }
  }

  io.input.ready := fsm.isActive(fsm.DATA) || fsm.isActive(fsm.FLUSH)

  io.rgmii.txc := ddrOut(True, False)

  for (i <- 0 until 4) {
    io.rgmii.txd(i) := ddrOut(txData(i), txData(i + 4))
  }

  val txctlRise = txDv
  val txctlFall = txDv ^ txEr
  io.rgmii.txctl := ddrOut(txctlRise, txctlFall)
}

// ============================================================================
// ECP5 BlackBox Primitives
// ============================================================================

case class IDDRX1F() extends BlackBox {
  val io = new Bundle {
    val D = in Bool ()
    val SCLK = in Bool ()
    val RST = in Bool ()
    val Q0 = out Bool ()
    val Q1 = out Bool ()
  }
  noIoPrefix()
  mapClockDomain(clock = io.SCLK)
}

case class ODDRX1F() extends BlackBox {
  val io = new Bundle {
    val D0 = in Bool ()
    val D1 = in Bool ()
    val SCLK = in Bool ()
    val RST = in Bool ()
    val Q = out Bool ()
  }
  noIoPrefix()
  mapClockDomain(clock = io.SCLK)
}

case class DELAYG(mode: String, value: Int) extends BlackBox {
  val io = new Bundle {
    val A = in Bool ()
    val Z = out Bool ()
  }
  noIoPrefix()
  addGeneric("DEL_MODE", mode)
  addGeneric("DEL_VALUE", value)
}
