package lzw.core.fixtures

import lzw.core.{CodeWidthOptions, Options}

object Fixtures {
  private val BytesOptions = Options(
    alphabet = Seq.range(0, 256).map(_.toByte),
    codeWidth = CodeWidthOptions.fixedWidth(12),
  )

  val Empty: Fixture[Byte] = Fixture(
    "empty",
    BytesOptions,
    symbols = Seq.empty,
    codesBits = Seq.empty,
    dictionarySizeAtTheEnd = 1 << java.lang.Byte.SIZE,
  )

  val FixedWidthCodes: Fixture[Byte] = Fixture(
    "fixed-width codes",
    BytesOptions,
    symbols = {
      val X = 'X'.toByte // b01011000
      val o = 'o'.toByte // b01101111
      Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X)
    },
    /*
      *************** ENCODING ***************
      Input | Output        | Dict
      ------+---------------+------------------
            |               | ...
            |               | b1011000: X
            |               | ...
            |               | b1101111: o
            |               | ...
            |               | b11111111: ...
      X     | b000001011000 | b100000000: Xo
      o     | b000001101111 | b100000001: oX
      Xo    | b000100000000 | b100000010: XoX
      XoX   | b000100000010 | b100000011: XoXo
      oX    | b000100000001 | b100000100: oXo
      oXo   | b000100000100 | b100000101: oXoX
      XoX   | b000100000010 |

      **************** DECODING ****************
      Input         | Output | Dict
      --------------+--------+------------------
                    |        | ...
                    |        | b1011000: X
                    |        | ...
                    |        | b1101111: o
                    |        | ...
                    |        | b11111111: ...
      b000001011000 | X      |
      b000001101111 | o      | b100000000: Xo
      b000100000000 | Xo     | b100000001: oX
      b000100000010 | ?      | b100000010: ?
                    | XoX    | b100000010: XoX
      b000100000001 | oX     | b100000011: XoXo
      b000100000100 | ?      | b100000100: ?
                    | oXo    | b100000100: oXo
      b000100000010 | XoX    | b100000101: oXoX
     */
    codesBits = Seq(
      "000001011000",
      "000001101111",
      "000100000000",
      "000100000010",
      "000100000001",
      "000100000100",
      "000100000010",
    ),
    dictionarySizeAtTheEnd = (1 << java.lang.Byte.SIZE) + 6,
  )
}
