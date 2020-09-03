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

  val X = 'X'
  val o = 'o'
  val OptionsForVariableWidthCodes: Options[Char] = Options(
    alphabet = Seq(X, o),
    codeWidth = CodeWidthOptions(initialWidth = 2, maximumWidth = None)
  )

  val VariableWidthCodes: Fixture[Char] = Fixture(
    "variable-width codes",
    OptionsForVariableWidthCodes,
    symbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X),
    /*
      **************** ENCODING ****************
      Input | Output | Dict         | Code width
      ------+--------+--------------+-----------
            |        | b0: X        |
            |        | b1: o        | 2
      X     | b00    | b10: Xo      |
      o     | b01    | b11: oX      |
      Xo    | b10    | b100: XoX    | 3
      XoX   | b100   | b101: XoXo   |
      oX    | b011   | b110: oXo    |
      oXo   | b110   | b111: oXoX   |
      XoXo  | b101   | b1000: XoXoX | 4
      XoX   | b0100  |

      **************** DECODING ****************
      Input | Output | Dict         | Code width
      ------+--------+--------------+-----------
            |        | b0: X        |
            |        | b1: o        | 2
      b00   | X      |              |
      b01   | o      | b10: Xo      |
      b10   | Xo     | b11: oX      | 3
      b100  | ?      | b100: ?      |
            | XoX    | b100: XoX    |
      b011  | oX     | b101: XoXo   |
      b110  | ?      | b110: ?      |
            | oXo    | b110: oXo    |
      b101  | XoXo   | b111: oXoX   | 4
      b0100 | XoX    | b1000: XoXoX |
     */
    codesBits = Seq(
      "00",
      "01",
      "10",
      "100",
      "011",
      "110",
      "101",
      "0100",
    ),
    dictionarySizeAtTheEnd = 9,
  )

  val VariableWidthCodesWithEarlyChange: Fixture[Char] = Fixture(
    "variable-width codes with early change",
    OptionsForVariableWidthCodes.copy(
      codeWidth = OptionsForVariableWidthCodes.codeWidth.copy(
        earlyChange = true
      )
    ),
    symbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X),
    /*
      **************** ENCODING ****************
      Input | Output | Dict         | Code width
      ------+--------+--------------+-----------
            |        | b0: X        |
            |        | b1: o        | 2
      X     | b00    | b10: Xo      |
      o     | b01    | b11: oX      | 3
      Xo    | b010   | b100: XoX    |
      XoX   | b100   | b101: XoXo   |
      oX    | b011   | b110: oXo    |
      oXo   | b110   | b111: oXoX   | 4
      XoXo  | b0101  | b1000: XoXoX |
      XoX   | b0100  |

      * DECODING *
      Input | Output | Dict         | Code width
      ------+--------+--------------+-----------
            |        | b0: X        |
            |        | b1: o        | 2
      b00   | X      |              |
      b01   | o      | b10: Xo      | 3
      b010  | Xo     | b11: oX      |
      b100  | ?      | b100: ?      |
            | XoX    | b100: XoX    |
      b011  | oX     | b101: XoXo   |
      b110  | ?      | b110: ?      |
            | oXo    | b110: oXo    | 4
      b0101 | XoXo   | b111: oXoX   |
      b0100 | XoX    | b1000: XoXoX |
     */
    codesBits = Seq(
      "00",
      "01",
      "010",
      "100",
      "011",
      "110",
      "0101",
      "0100",
    ),
    dictionarySizeAtTheEnd = 9,
  )
}
