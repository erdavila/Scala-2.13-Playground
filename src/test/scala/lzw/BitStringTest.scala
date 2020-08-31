package lzw

import org.scalatest.funsuite.AnyFunSuite

class BitStringTest extends AnyFunSuite {
  test("from()") {
    val bsByte = BitString.from(123.toByte)
    assert(bsByte.length == 8)
    assert(bsByte.toString == "01111011")
    assert(bsByte.toString(8) == "01111011")

    val bsShort = BitString.from(12345.toShort)
    assert(bsShort.length == 16)
    assert(bsShort.toString() == "0011000000111001")
    assert(bsShort.toString(8) == "00110000 00111001")

    val bsInt = BitString.from(1234567890.toInt)
    assert(bsInt.length == 32)
    assert(bsInt.toString() == "01001001100101100000001011010010")
    assert(bsInt.toString(8) == "01001001 10010110 00000010 11010010")

    val bsLong = BitString.from(1234567890123456789L)
    assert(bsLong.length == 64)
    assert(bsLong.toString() == "0001000100100010000100001111010001111101111010011000000100010101")
    assert(bsLong.toString(8) == "00010001 00100010 00010000 11110100 01111101 11101001 10000001 00010101")
  }

  test("splitLsbAt()") {
    val bs = BitString.from(1234567890123456789L)

    {
      val (left, right) = bs.splitLsbAt(0)
      assert(left == bs)
      assert(right == BitString.empty)
    }

    {
      val (left, right) = bs.splitLsbAt(12)
      assert(left.toString == "0001000100100010000100001111010001111101111010011000")
      assert(right.toString == "000100010101")
    }

    {
      val (left, right) = bs.splitLsbAt(64)
      assert(left == BitString.empty)
      assert(right == bs)
    }

    {
      val (left, right) = bs.splitLsbAt(65)
      assert(left == BitString.empty)
      assert(right == bs)
    }
  }
}
