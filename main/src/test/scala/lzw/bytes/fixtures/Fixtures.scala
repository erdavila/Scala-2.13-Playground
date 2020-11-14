package lzw.bytes.fixtures

import lzw.bytes.{CodeWidthOptions, Options}

object Fixtures {
  val Empty: Fixture = Fixture(
    "empty",
    Options(
      codeWidth = CodeWidthOptions.fixedWidth(12)
    ),
    decodedBytes = Array.emptyByteArray,
    encodedBytesLsb = Array.emptyByteArray,
    encodedBytesMsb = Array.emptyByteArray
  )

  val FixedWidthCodes: Fixture = Fixture(
    "fixed-width codes",
    Options(
      codeWidth = CodeWidthOptions.fixedWidth(12)
    ),
    decodedBytes = {
      val X = 'X'.toByte // b01011000
      val o = 'o'.toByte // b01101111
      Array(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X)
    },
    /*
      ******************************************* ENCODING *******************************************
      Match | Input  | Output        | Dict             | LSB buff |   LSB out | MSB out   | MSB buff
      ------+--------+---------------+------------------+----------+-----------+-----------+---------
            |        |               | ...              |          |           |           |
            |        |               | b1011000: X      |          |           |           |
            |        |               | ...              |          |           |           |
            |        |               | b1101111: o      |          |           |           |
            |        |               | ...              |          |           |           |
            |        |               | b11111111: ...   |          |           |           |
            | X      |               |                  |          |           |           |
      X     | o      | b000001011000 | b100000000: Xo   |    b0000 | b01011000 | b00000101 | b1000
      o     | X      | b000001101111 | b100000001: oX   |          | b11110000 | b10000000 |
            |        |               |                  |          | b00000110 | b01101111 |
      X     | o      |               |                  |          |           |           |
      Xo    | X      | b000100000000 | b100000010: XoX  |    b0001 | b00000000 | b00010000 | b0000
      X     | o      |               |                  |          |           |           |
      Xo    | X      |               |                  |          |           |           |
      XoX   | o      | b000100000010 | b100000011: XoXo |          | b00100001 | b00000001 |
            |        |               |                  |          | b00010000 | b00000010 |
      o     | X      |               |                  |          |           |           |
      oX    | o      | b000100000001 | b100000100: oXo  |    b0001 | b00000001 | b00010000 | b0001
      o     | X      |               |                  |          |           |           |
      oX    | o      |               |                  |          |           |           |
      oXo   | X      | b000100000100 | b100000101: oXoX |          | b01000001 | b00010001 |
            |        |               |                  |          | b00010000 | b00000100 |
      X     | o      |               |                  |          |           |           |
      Xo    | X      |               |                  |          |           |           |
      XoX   | finish | b000100000010 |                  |          | b00000010 | b00010000 |
            |        |               |                  |          | b****0001 | b0010**** |
     */
    encodedBytesLsb = Array(88, 240, 6, 0, 33, 16, 1, 65, 16, 2, 1).map(_.toByte),
    encodedBytesMsb = Array(5, 128, 111, 16, 1, 2, 16, 17, 4, 16, 32).map(_.toByte)
  )
}
