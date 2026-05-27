import spinal.lib.com.uart._

package object testing {
  val uartConfig = UartCtrlInitConfig(
    baudrate = 1_562_500,
    dataLength = 7,
    parity = UartParityType.NONE,
    stop = UartStopType.ONE
  )
}
