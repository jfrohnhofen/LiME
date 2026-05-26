package lime

import spinal.core._

package object util {
  type Byte = Bits
  def Byte = Bits(8 bits)

  implicit class BitsByteAccess(val x: Bits) {
    def takeHigh(n: Int): Bits = x(x.high downto x.getWidth - n)
    def byte(index: Int): Bits = {
      val high = index * 8 + 7
      val low = index * 8
      x(high downto low)
    }
    def byte(index: UInt): Bits = {
      x.subdivideIn(8 bits)(index)
    }

    def nibble(index: Int): Bits = {
      val high = index * 4 + 3
      val low = index * 4
      x(high downto low)
    }
    def nibble(index: UInt): Bits = {
      x.subdivideIn(4 bits)(index)
    }
  }

  implicit class UIntByteAccess(val x: UInt) {
    def byte(index: Int): UInt = {
      val high = index * 8 + 7
      val low = index * 8
      x(high downto low)
    }
    def byte(index: UInt): UInt = {
      x.asBits.subdivideIn(8 bits)(index).asUInt
    }

    def nibble(index: Int): UInt = {
      val high = index * 4 + 3
      val low = index * 4
      x(high downto low)
    }
    def nibble(index: UInt): UInt = {
      x.asBits.subdivideIn(4 bits)(index).asUInt
    }
  }

  implicit class ShiftAppend(val x: Bits) {

    def #<<(y: BitVector): Bits = {
      val shiftAmount = y.getBitsWidth
      val currentWidth = x.getBitsWidth

      require(currentWidth >= shiftAmount, s"Cannot shift in $shiftAmount bits into a $currentWidth-bit vector")

      if (currentWidth == shiftAmount) {
        y.asBits
      } else {
        x(x.high - shiftAmount downto 0) ## y
      }
    }
  }
}
