package lzw.bits

import org.scalatest.funsuite.AnyFunSuite

class BitStringTest extends AnyFunSuite {
  test("from()") {
    val bsByte = BitString.from(123.toByte)
    assert(bsByte.length == 8)
    assert(bsByte.toString == "01111011")
    assert(bsByte.toString(8) == "01111011")

    val bsShort = BitString.from(12345.toShort)
    assert(bsShort.length == 16)
    assert(bsShort.toString == "0011000000111001")
    assert(bsShort.toString(8) == "00110000 00111001")

    val bsInt = BitString.from(1234567890.toInt)
    assert(bsInt.length == 32)
    assert(bsInt.toString == "01001001100101100000001011010010")
    assert(bsInt.toString(8) == "01001001 10010110 00000010 11010010")

    val bsLong = BitString.from(1234567890123456789L)
    assert(bsLong.length == 64)
    assert(bsLong.toString == "0001000100100010000100001111010001111101111010011000000100010101")
    assert(bsLong.toString(8) == "00010001 00100010 00010000 11110100 01111101 11101001 10000001 00010101")
  }

  test("parse()") {
    {
      val bs = BitString.parse("")
      assert(bs.length == 0)
      assert(bs.toString == "")
    }

    {
      val bs = BitString.parse("1001")
      assert(bs.length == 4)
      assert(bs.toString == "1001")
    }

    {
      val bs = BitString.parse("10010101110010100101011010101010")
      assert(bs.length == 32)
      assert(bs.toString == "10010101110010100101011010101010")
    }

    {
      val bs = BitString.parse("100101011100101001010110101010100")
      assert(bs.length == 33)
      assert(bs.toString == "100101011100101001010110101010100")
    }

    {
      val bs = BitString.parse("10010101110010100101011010101010010011001010010110010010101101010011")
      assert(bs.length == 68)
      assert(bs.toString == "10010101110010100101011010101010010011001010010110010010101101010011")
    }
  }

  test("lsb.splitAt()") {
    val bs = BitString.from(1234567890123456789L)

    {
      val (left, right) = bs.lsb.splitAt(0)
      assert(left == BitString.empty)
      assert(right == bs)
    }

    {
      val (left, right) = bs.lsb.splitAt(12)
      assert(left.toString == "000100010101")
      assert(right.toString == "0001000100100010000100001111010001111101111010011000")
    }

    {
      val (left, right) = bs.lsb.splitAt(64)
      assert(right == BitString.empty)
      assert(left == bs)
    }

    {
      val (left, right) = bs.lsb.splitAt(65)
      assert(right == BitString.empty)
      assert(left == bs)
    }
  }

  test("lsb.bytesIterator") {
    { // 0 bits
      val bs = BitString.parse("")
      val expectedBytes = Seq.empty
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 1 bit
      val bs = BitString.parse("1")
      val expectedBytes = Seq(
        1, // *******1
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 7 bits
      val bs = BitString.parse("0011001")
      val expectedBytes = Seq(
         25, // *0011001
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 8 bits
      val bs = BitString.parse("10011001")
      val expectedBytes = Seq(
         -103, // 10011001
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 9 bits
      val bs = BitString.parse("010011001")
      val expectedBytes = Seq(
         -103, // 10011001
            0, // *******0
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 31 bits
      val bs = BitString.parse("0010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
           20, // *0010100
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 32 bits
      val bs = BitString.parse("10010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 33 bits
      val bs = BitString.parse("110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
            1, // *******1
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 34 bits
      val bs = BitString.parse("1110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
            3, // ******11
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 35 bits
      val bs = BitString.parse("01110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
            3, // *****011
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 36 bits
      val bs = BitString.parse("101110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
           11, // ****1011
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 37 bits
      val bs = BitString.parse("0101110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
           11, // ***01011
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 38 bits
      val bs = BitString.parse("10101110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
           43, // **101011
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 39 bits
      val bs = BitString.parse("010101110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
           43, // *0101011
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 40 bits
      val bs = BitString.parse("0010101110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
           43, // 00101011
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }

    { // 41 bits
      val bs = BitString.parse("10010101110010100101011010101010010011001")
      val expectedBytes = Seq(
         -103, // 10011001
           84, // 01010100
          -83, // 10101101
         -108, // 10010100
           43, // 00101011
            1, // *******1
      )
      assert(bs.lsb.bytesIterator.toSeq == expectedBytes)
    }
  }

  test("msb.bytesIterator") {
    { // 0 bits
      val bs = BitString.parse("")
      val expectedBytes = Seq.empty
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 1 bit
      val bs = BitString.parse("1")
      val expectedBytes = Seq(
        -128, // 1*******
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 7 bits
      val bs = BitString.parse("1001010")
      val expectedBytes = Seq(
        -108, // 1001010*
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 8 bits
      val bs = BitString.parse("10010101")
      val expectedBytes = Seq(
        -107, // 10010101
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 9 bits
      val bs = BitString.parse("100101011")
      val expectedBytes = Seq(
        -107, // 10010101
        -128, // 1*******
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 31 bits
      val bs = BitString.parse("1001010111001010010101101010101")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 1010101*
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 32 bits
      val bs = BitString.parse("10010101110010100101011010101010")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 33 bits
      val bs = BitString.parse("100101011100101001010110101010100")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
           0, // 0*******
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 34 bits
      val bs = BitString.parse("1001010111001010010101101010101001")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          64, // 01******
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 35 bits
      val bs = BitString.parse("10010101110010100101011010101010010")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          64, // 010*****
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 36 bits
      val bs = BitString.parse("100101011100101001010110101010100100")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          64, // 0100****
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 37 bits
      val bs = BitString.parse("1001010111001010010101101010101001001")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          72, // 01001***
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 38 bits
      val bs = BitString.parse("10010101110010100101011010101010010011")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          76, // 010011**
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 39 bits
      val bs = BitString.parse("100101011100101001010110101010100100110")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          76, // 0100110*
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 40 bits
      val bs = BitString.parse("1001010111001010010101101010101001001100")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          76, // 01001100
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }

    { // 41 bits
      val bs = BitString.parse("10010101110010100101011010101010010011001")
      val expectedBytes = Seq(
        -107, // 10010101
         -54, // 11001010
          86, // 01010110
         -86, // 10101010
          76, // 01001100
        -128, // 1*******
      )
      assert(bs.msb.bytesIterator.toSeq == expectedBytes)
    }
  }

  testsFor(
    concat("EmptyBitString", "EmptyBitString", "EmptyBitString")(
      "",
      ""
    )
  )

  testsFor(
    concat("EmptyBitString", "WordBitString", "WordBitString")(
      "",
      "001100100110110010",
    )
  )

  testsFor(
    concat("EmptyBitString", "WordArrayBitString", "WordArrayBitString")(
      "",
      "0110100101110101010001011010011011000100111010110"
    )
  )

  testsFor(
    concat("WordBitString", "EmptyBitString", "WordBitString")(
      "001100100110110010",
      "",
    )
  )

  testsFor(
    concat("WordBitString", "WordBitString", "WordBitString")(
      "001100100110110",
      "001001001101100",
    )
  )

  testsFor(
    concat("WordBitString", "WordBitString", "WordArrayBitString")(
      "00110010011011100101010",
      "001001001101101000101100100",
    )
  )

  testsFor(
    concat("WordBitString", "WordArrayBitString", "WordArrayBitString")(
      "001100100110110",
      "0110100101110101010001011010011011000100111010110",
    )
  )

  testsFor(
    concat("WordArrayBitString", "EmptyBitString", "WordArrayBitString")(
      "0110100101110101010001011010011011000100111010110",
      "",
    )
  )

  testsFor(
    concat("WordArrayBitString", "WordBitString", "WordArrayBitString")(
      "0110100101110101010001011010011011000100111010110",
      "001100100110110",
    )
  )

  testsFor(
    concat("WordArrayBitString", "WordArrayBitString", "WordArrayBitString")(
      "0110100101110101010001011010011011000100111010110",
      "0011001001101101101010010011001010100101011",
    )
  )

  private def concat(lhsType: String, rhsType: String, expectedType: String)(lhsBits: String, rhsBits: String): Unit = {
    def requireBitsType(bits: String, `type`: String): Unit =
      `type` match {
        case "EmptyBitString" => require(bits.isEmpty)
        case "WordBitString" => require(bits.nonEmpty); require(bits.lengthIs <= BitString.WordSize)
        case "WordArrayBitString" => require(bits.lengthIs > BitString.WordSize)
      }

    requireBitsType(lhsBits, lhsType)
    requireBitsType(rhsBits, rhsType)

    test(s"(_: $lhsType) `concat` (_: $rhsType) == (_: $expectedType)") {
      val lhs = BitString.parse(lhsBits)
      val rhs = BitString.parse(rhsBits)
      val expected = BitString.parse(lhsBits ++ rhsBits)
      assert(lhs ++ rhs == expected)
    }
  }
}
