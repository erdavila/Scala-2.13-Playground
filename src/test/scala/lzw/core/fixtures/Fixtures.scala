package lzw.core.fixtures

import lzw.core.fixtures.SteppedEncodingFixture.{Encode, Finish, Reset}
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

  val VariableWidthCodesWithMaxWidth: SteppedEncodingFixture[Char] = SteppedEncodingFixture(
    "variable-width codes with max width",
    OptionsForVariableWidthCodes.copy(
      codeWidth = OptionsForVariableWidthCodes.codeWidth.copy(
        maximumWidth = Some(3)
      )
    ),
    /*
      *************** ENCODING ***************
      Input | Output | Dict       | Code width
      ------+--------+------------+----------
            |        | b0: X      |
            |        | b1: o      | 2
      X     | b00    | b10: Xo    |
      o     | b01    | b11: oX    |
      Xo    | b10    | b100: XoX  | 3
      XoX   | b100   | b101: XoXo |
      oX    | b011   | b110: oXo  |
      oXo   | b110   | b111: oXoX |
      XoXo  | b101   | *AT MAX*   | 3
      XoXo  | b101   |
      XoXo  | b101   |

      *************** DECODING ***************
      Input | Output | Dict       | Code width
      ------+--------+------------+----------
            |        | b0: X      |
            |        | b1: o      | 2
      b00   | X      |            |
      b01   | o      | b10: Xo    |
      b10   | Xo     | b11: oX    | 3
      b100  | ?      | b100: ?    |
            | XoX    | b100: XoX  |
      b011  | oX     | b101: XoXo |
      b110  | ?      | b110: ?    |
            | oXo    | b110: oXo  |
      b101  | XoXo   | b111: oXoX | 3
      b101  | XoXo   | *AT MAX*   |
      b101  | XoXo   |
     */
    Seq(
      Encode(
        Seq(X, o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("00", "01", "10", "100", "011"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, false),
          (_.statistics.inputSymbols, 12),
          (_.statistics.outputBits, 12),
          (_.statistics.outputCodes, 5),
          (_.statistics.dictionarySize, 7),
        ),
      ),
      Encode(
        Seq(X),
        expectedBits = Seq("110"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, /*CHANGED*/true),
          (_.statistics.inputSymbols, 13),
          (_.statistics.outputBits, 15),
          (_.statistics.outputCodes, 6),
          (_.statistics.dictionarySize, 8),
        ),
      ),
      Encode(
        Seq(o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("101", "101"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, true),
          (_.statistics.inputSymbols, 24),
          (_.statistics.outputBits, 21),
          (_.statistics.outputCodes, 8),
          (_.statistics.dictionarySize, 8),
        ),
      ),
      Finish(
        expectedBits = Seq("101"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, true),
          (_.statistics.inputSymbols, 24),
          (_.statistics.outputBits, 24),
          (_.statistics.outputCodes, 9),
          (_.statistics.dictionarySize, 8),
        ),
      )
    ),
    dictionarySizeAtTheEnd = 8,
  )

  val VariableWidthCodesWithMaxWidthAndEarlyChange: SteppedEncodingFixture[Char] = SteppedEncodingFixture(
    "variable-width codes with max width and early change",
    OptionsForVariableWidthCodes.copy(
      codeWidth = OptionsForVariableWidthCodes.codeWidth.copy(
        maximumWidth = Some(3),
        earlyChange = true,
      )
    ),
    /*
      *************** ENCODING ***************
      Input | Output | Dict       | Code width
      ------+--------+------------+-----------
            |        | b0: X      |
            |        | b1: o      | 2
      X     | b00    | b10: Xo    |
      o     | b01    | b11: oX    | 3
      Xo    | b010   | b100: XoX  |
      XoX   | b100   | b101: XoXo |
      oX    | b011   | b110: oXo  |
      oXo   | b110   | b111: oXoX | 3
      XoXo  | b101   | *AT MAX*
      XoXo  | b101   |
      XoXo  | b101   |

      *************** DECODING ***************
      Input | Output | Dict       | Code width
      ------+--------+------------+-----------
            |        | b0: X      |
            |        | b1: o      | 2
      b00   | X      |            |
      b01   | o      | b10: Xo    | 3
      b010  | Xo     | b11: oX    |
      b100  | ?      | b100: ?    |
            | XoX    | b100: XoX  |
      b011  | oX     | b101: XoXo |
      b110  | ?      | b110: ?    |
            | oXo    | b110: oXo  | 3
      b101  | XoXo   | b111: oXoX |
      b101  | XoXo   | *AT MAX*
      b101  | XoXo
     */
    Seq(
      Encode(
        Seq(X, o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("00", "01", "010", "100", "011"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, false),
          (_.statistics.inputSymbols, 12),
          (_.statistics.outputBits, 13),
          (_.statistics.outputCodes, 5),
          (_.statistics.dictionarySize, 7),
        ),
      ),
      Encode(
        Seq(X),
        expectedBits = Seq("110"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, /*CHANGED*/true),
          (_.statistics.inputSymbols, 13),
          (_.statistics.outputBits, 16),
          (_.statistics.outputCodes, 6),
          (_.statistics.dictionarySize, 8),
        ),
      ),
      Encode(
        Seq(o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("101", "101"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, true),
          (_.statistics.inputSymbols, 24),
          (_.statistics.outputBits, 22),
          (_.statistics.outputCodes, 8),
          (_.statistics.dictionarySize, 8),
        ),
      ),
      Finish(
        expectedBits = Seq("101"),
        assertions = Seq(
          (_.maxCodeWidthExhausted, true),
          (_.statistics.inputSymbols, 24),
          (_.statistics.outputBits, 25),
          (_.statistics.outputCodes, 9),
          (_.statistics.dictionarySize, 8),
        ),
      ),
    ),
    dictionarySizeAtTheEnd = 8,
  )

  val MaxDictionarySize: SteppedEncodingFixture[Char] = SteppedEncodingFixture(
    "max dictionary size",
    Options(
      alphabet = Seq(X, o),
      codeWidth = CodeWidthOptions(initialWidth = 2, maximumWidth = None),
      maxDictionarySize = Some(5),
    ),
    /*
      ************** ENCODING **************
      Input | Output | Dict      | Code width
      ------+--------+-----------+----------
            |        | b0: X     |
            |        | b1: o     | 2
      X     | b00    | b10: Xo   |
      o     | b01    | b11: oX   |
      Xo    | b10    | b100: XoX | 3
      XoX   | b100   | *AT MAX*
      oX    | b011   |
      oX    | b011   |
      oX    | b011   |
      oX    | b011   |
      oX    | b011   |
      oX    | b011   |

      ************** DECODING **************
      Input | Output | Dict      | Code width
      ------+--------+-----------+-----------
            |        | b0: X     |
            |        | b1: o     | 2
      b00   | X      |           |
      b01   | o      | b10: Xo   |
      b10   | Xo     | b11: oX   | 3
      b100  | ?      | b100: ?   |
            | XoX    | b100: XoX |
      b011  | oX     | *AT MAX*
      b011  | oX
      b011  | oX
      b011  | oX
      b011  | oX
      b011  | oX
     */
    steps = Seq(
      Encode(
        Seq(X, o, X, o),
        expectedBits = Seq("00", "01"),
        assertions = Seq(
          (_.statistics.inputSymbols, 4),
          (_.statistics.outputBits, 4),
          (_.statistics.outputCodes, 2),
          (_.statistics.dictionarySize, 4),
        )
      ),
      Encode(
        Seq(X),
        expectedBits = Seq("10"),
        assertions = Seq(
          (_.statistics.inputSymbols, 5),
          (_.statistics.outputBits, 6),
          (_.statistics.outputCodes, 3),
          (_.statistics.dictionarySize, 5),
        )
      ),
      Encode(
        Seq(o, X, o, X, o, X, o),
        expectedBits = Seq("100", "011", "011"),
        assertions = Seq(
          (_.statistics.inputSymbols, 12),
          (_.statistics.outputBits, 15),
          (_.statistics.outputCodes, 6),
          (_.statistics.dictionarySize, 5),
        )
      ),
      Encode(
        Seq(X, o, X, o, X, o, X),
        expectedBits = Seq("011", "011", "011"),
        assertions = Seq(
          (_.statistics.inputSymbols, 19),
          (_.statistics.outputBits, 24),
          (_.statistics.outputCodes, 9),
          (_.statistics.dictionarySize, 5),
        )
      ),
      Finish(
        expectedBits = Seq("011"),
        assertions = Seq(
          (_.statistics.inputSymbols, 19),
          (_.statistics.outputBits, 27),
          (_.statistics.outputCodes, 10),
          (_.statistics.dictionarySize, 5),
        )
      ),
    ),
    dictionarySizeAtTheEnd = 5,
  )

  val ClearCode: SteppedEncodingFixture[Char] = SteppedEncodingFixture(
    "clear code",
    Options(
      alphabet = Seq(X, o),
      codeWidth = CodeWidthOptions(initialWidth = 2, maximumWidth = None),
      clearCode = Some(1),
    ),
    /*
        ***************** ENCODING *****************
        Matched |       |        |           | Code
        before  | Input | Output | Dict      | width
        --------+-------+--------+-----------+------
                |       |        | b0: X     | 2
                |       |        | b1: CLEAR |
                |       |        | b10: o    |
                | X     |        |           |
        X       | o     | b00    | b11: Xo   |
        o       | X     | b10    | b100: oX  | 3
        X       | o     |        |           |
        Xo      | X     | b011   | b101: XoX |
        X       | o     |        |           |
        Xo      | X     |        |           |
        XoX     | CLEAR | b101   |           |
                |       | b001   |-----------|------
                |       |        | b0: X     | 2
                |       |        | b1: CLEAR |
                |       |        | b10: o    |
                | o     |        |           |
        o       | X     | b10    | b11: oX   |
        X       | o     | b00    | b100: Xo  | 3
        o       | X     |        |           |
        oX      | END   | b011   |           |

        ************ DECODING ************
              |        |           | Code
        Input | Output | Dict      | width
        ------+--------+-----------+------
              |        | b0: X     | 2
              |        | b1: CLEAR |
              |        | b10: o    |
        b00   | X      |           |
        b10   | o      | b11: Xo   | 3
        b011  | Xo     | b100: oX  |
        b101  | ?      | b101: ?   |
              | XoX    | b101: XoX |
        b001  | CLEAR  |-----------|------
              |        | b0: X     | 2
              |        | b1: CLEAR |
              |        | b10: o    |
        b10   | o      |           |
        b00   | X      | b11: oX   | 3
        b011  | oX     | b100: Xo  |
     */
    steps = Seq(
      Encode(
        Seq('X', 'o', 'X', 'o', 'X', 'o', 'X'),
        expectedBits = Seq("00", "10", "011"),
        assertions = Seq(
          (_.statistics.inputSymbols, 7),
          (_.statistics.outputBits, 7),
          (_.statistics.outputCodes, 3),
          (_.statistics.dictionarySize, 5),
        )
      ),
      Reset(
        expectedBits = Seq("101", "001"),
        assertions = Seq(
          (_.statistics.inputSymbols, 7),
          (_.statistics.outputBits, 13),
          (_.statistics.outputCodes, 5),
          (_.statistics.dictionarySize, 2),
        )
      ),
      Encode(
        Seq('o', 'X', 'o', 'X'),
        expectedBits = Seq("10", "00"),
        assertions = Seq(
          (_.statistics.inputSymbols, 11),
          (_.statistics.outputBits, 17),
          (_.statistics.outputCodes, 7),
          (_.statistics.dictionarySize, 4),
        )
      ),
      Finish(
        expectedBits = Seq("011"),
        assertions = Seq(
          (_.statistics.inputSymbols, 11),
          (_.statistics.outputBits, 20),
          (_.statistics.outputCodes, 8),
          (_.statistics.dictionarySize, 4),
        )
      ),
    ),
    dictionarySizeAtTheEnd = 4,
  )

  val StopCode: Fixture[Char] = Fixture(
    "stop code",
    Options(
      alphabet = Seq(X, o),
      codeWidth = CodeWidthOptions(initialWidth = 4, maximumWidth = None),
      stopCode = Some(1),
    ),
    symbols = Seq(X, o, X, o, X, o, X),
    /*
      ************** ENCODING **************
      Matched | Input | Output | Dictionary
      --------+-------+--------+-----------
              |       |        | b0: X
              |       |        | b1: STOP
              |       |        | b10: o
              | X     |        |
      X       | o     | b0000  | b11: Xo
      o       | X     | b0010  | b100: oX
      X       | o     |        |
      Xo      | X     | b0011  | b101: XoX
      X       | o     |        |
      Xo      | X     |        |
      XoX     | STOP  | b0101  |
              |       | b0001  |

      ** DECODING **
      Input | Output | Dictionary
      ------+--------+-----------
            |        | b0: X
            |        | b1: STOP
            |        | b10: o
      b0000 | X      |
      b0010 | o      | b11: Xo
      b0011 | Xo     | b100: oX
      b0101 | ?      | b101: ?
            | XoX    | b101: XoX
      b0001 | STOP

     */
    codesBits = Seq(
      "0000",
      "0010",
      "0011",
      "0101",
      "0001",
    ),
    dictionarySizeAtTheEnd = 5,
  )

  val ClearCodeAndStopCode: SteppedEncodingFixture[Char] = SteppedEncodingFixture(
    "clear code and stop code",
    Options(
      alphabet = Seq(X, o),
      codeWidth = CodeWidthOptions.fixedWidth(4),
      clearCode = Some(3),
      stopCode = Some(5),
    ),
    /*
      ************** ENCODING **************
      Matched | Input | Output | Dictionary
      --------+-------+--------+-----------
              |       |        | b0: X
              |       |        | b1: o
              |       |        | b10: !
              |       |        | b11: CLEAR
              |       |        | b100: !
              |       |        | b101: STOP
              | X     |        |
      X       | o     | b0000  | b10: Xo
      o       | X     | b0001  | b100: oX
      X       | o     |        |
      Xo      | X     | b0010  | b110: XoX
      X       | o     |        |
      Xo      | X     |        |
      XoX     | o     | b0110  | b111: XoXo
      o       | X     |        |
      oX      | CLEAR | b0100  |
              |       | b0011  |-----------
              |       |        | b0: X
              |       |        | b1: o
              |       |        | b10: !
              |       |        | b11: CLEAR
              |       |        | b100: !
              |       |        | b101: STOP
              | o     |        |
      o       | X     | b0001  | b10: oX
      X       | STOP  | b0000  |
              |       | b0101  |

      ********* DECODING *********
      Input | Output | Dictionary
      ------+--------+-----------
            |        | b0: X
            |        | b1: o
            |        | b10: !
            |        | b11: CLEAR
            |        | b100: !
            |        | b101: STOP
      b0000 | X      |
      b0001 | o      | b10: Xo
      b0010 | Xo     | b100: oX
      b0110 | ?      | b110: ?
            | XoX    | b110: XoX
      b0100 | oX     | b111: XoXo
      b0011 | CLEAR  |-----------
            |        | b0: X
            |        | b1: o
            |        | b10: !
            |        | b11: CLEAR
            |        | b100: !
            |        | b101: STOP
      b0001 | o      |
      b0000 | X      | b10: oX
      b0101 | STOP
     */
    steps = Seq(
      Encode(
        Seq(X, o, X, o, X, o, X, o, X),
        expectedBits = Seq("0000", "0001", "0010", "0110"),
        assertions = Seq(
          (_.statistics.inputSymbols, 9),
          (_.statistics.outputCodes, 4),
          (_.statistics.outputBits, 16),
          (_.statistics.dictionarySize, 6),
        )
      ),
      Reset(
        expectedBits = Seq("0100", "0011"),
        assertions = Seq(
          (_.statistics.inputSymbols, 9),
          (_.statistics.outputCodes, 6),
          (_.statistics.outputBits, 24),
          (_.statistics.dictionarySize, 2),
        )
      ),
      Encode(
        Seq(o, X),
        expectedBits = Seq("0001"),
        assertions = Seq(
          (_.statistics.inputSymbols, 11),
          (_.statistics.outputCodes, 7),
          (_.statistics.outputBits, 28),
          (_.statistics.dictionarySize, 3),
        )
      ),
      Finish(
        expectedBits = Seq("0000", "0101"),
        assertions = Seq(
          (_.statistics.inputSymbols, 11),
          (_.statistics.outputCodes, 9),
          (_.statistics.outputBits, 36),
          (_.statistics.dictionarySize, 3),
        )
      ),
    ),
    dictionarySizeAtTheEnd = 3,
  )
}
