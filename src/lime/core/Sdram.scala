package lime.core

import spinal.core._
import spinal.lib._
import spinal.lib.io._
import spinal.lib.memory.sdram._
import spinal.lib.memory.sdram.sdr._

case class Sdram(l: SdramLayout = M12L64322A.layout) extends Bundle with IMasterSlave {
  val addr = Bits(l.chipAddressWidth bits)
  val ba = Bits(l.bankWidth bits)
  val dq = Analog(Bits(l.dataWidth bits))
  val ras_n = Bool()
  val cas_n = Bool()
  val we_n = Bool()
  val clk = Bool()

  override def asMaster(): Unit = {
    out(addr, ba, ras_n, cas_n, we_n, clk)
    inout(dq)
  }
}

class SdramCtrl[T <: Data](contextType: T = NoData()) extends Component {
  val io = new Bundle {
    val sdram = master(Sdram())
    val sdramClk = in Bool ()
    val cmd = slave(Stream(SdramCtrlCmd(M12L64322A.layout, contextType)))
    val rsp = master(Stream(SdramCtrlRsp(M12L64322A.layout, contextType)))
  }

  val ctrl = spinal.lib.memory.sdram.sdr.SdramCtrl(
    M12L64322A.layout,
    M12L64322A.timings,
    CAS = 3,
    contextType = contextType
  )

  ctrl.io.bus.cmd << io.cmd.m2sPipe()
  io.rsp << ctrl.io.bus.rsp.s2mPipe()

  io.sdram.addr := ctrl.io.sdram.ADDR
  io.sdram.ba := ctrl.io.sdram.BA
  io.sdram.ras_n := ctrl.io.sdram.RASn
  io.sdram.cas_n := ctrl.io.sdram.CASn
  io.sdram.we_n := ctrl.io.sdram.WEn
  io.sdram.clk := io.sdramClk

  for (i <- 0 until 32) {
    val bb = BB()
    bb.io.I := ctrl.io.sdram.DQ.write(i)
    bb.io.T := !ctrl.io.sdram.DQ.writeEnable(i)
    ctrl.io.sdram.DQ.read(i) := bb.io.O
    bb.io.B <> io.sdram.dq(i)
  }
}

protected object M12L64322A {
  val layout = SdramLayout(
    generation = SdramGeneration.SDR,
    bankWidth = 2,
    columnWidth = 8,
    rowWidth = 11,
    dataWidth = 32
  )

  val timings = SdramTimings(
    bootRefreshCount = 8,
    tPOW = 200 us,
    tREF = 64 ms, // Total refresh period
    tRC = 70 ns, // Row cycle time
    tRFC = 70 ns, // Refresh cycle time
    tRAS = 48 ns, // Row active time
    tRP = 20 ns, // Row precharge time
    tRCD = 20 ns, // Row to column delay
    cMRD = 2, // Mode register delay
    tWR = 20 ns, // Write recovery time
    cWR = 2 // Write recovery cycles
  )
}

protected case class BB() extends BlackBox {
  val io = new Bundle {
    val I = in Bool ()
    val T = in Bool ()
    val O = out Bool ()
    val B = inout(Analog(Bool()))
  }
  noIoPrefix()
}
