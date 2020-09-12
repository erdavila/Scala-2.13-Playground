package lzw.bits

import org.scalatest.funsuite.AnyFunSuite

class BitUtilsTest extends AnyFunSuite {
  test("lsbMask") {
    assert(BitUtils.lsbMask(0) == 0)
    assert(BitUtils.lsbMask(1) == 1)
    assert(BitUtils.lsbMask(2) == 3)
    assert(BitUtils.lsbMask(3) == 7)
    assert(BitUtils.lsbMask(4) == 15)
    assert(BitUtils.lsbMask(5) == 31)
    assert(BitUtils.lsbMask(6) == 63)
    assert(BitUtils.lsbMask(7) == 127)
    assert(BitUtils.lsbMask(8) == 255)
  }

  test("bitsRequired") {
    assert(BitUtils.bitsRequired(0) == 0)
    assert(BitUtils.bitsRequired(1) == 1)
    assert(BitUtils.bitsRequired(2) == 2)
    assert(BitUtils.bitsRequired(3) == 2)
    assert(BitUtils.bitsRequired(4) == 3)
    assert(BitUtils.bitsRequired(5) == 3)
    assert(BitUtils.bitsRequired(6) == 3)
    assert(BitUtils.bitsRequired(7) == 3)
    assert(BitUtils.bitsRequired(8) == 4)
    assert(BitUtils.bitsRequired(15) == 4)
    assert(BitUtils.bitsRequired(16) == 5)
    assert(BitUtils.bitsRequired(31) == 5)
    assert(BitUtils.bitsRequired(32) == 6)
    assert(BitUtils.bitsRequired(63) == 6)
    assert(BitUtils.bitsRequired(64) == 7)
  }
}
