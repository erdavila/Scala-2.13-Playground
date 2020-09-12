package lzw.core

import lzw.bits.BitString
import org.scalatest.funsuite.AnyFunSuite

class LzwEncoderTest extends AnyFunSuite {
  private val bytesOptions = Options(
    alphabet = Seq.range(0, 256).map(_.toByte),
    codeWidth = CodeWidthOptions.fixedWidth(12),
  )

  testsFor(encoding("empty", bytesOptions, Seq.empty, Seq.empty))

  testsFor(
    encoding(
      "fixed-width codes",
      bytesOptions,
      inputSymbols = {
        val X = 'X'.toByte // b01011000
        val o = 'o'.toByte // b01101111
        Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X)
      },
      /*
        Input | Output        | Dict
        ------+---------------+------------------
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
       */
      expectedBits = Seq(
        "000001011000",
        "000001101111",
        "000100000000",
        "000100000010",
        "000100000001",
        "000100000100",
        "000100000010",
      ),
    )
  )

  private val X = 'X'
  private val o = 'o'
  private val optionsWithVariableWidthCodes = Options(
    alphabet = Seq(X, o),
    codeWidth = CodeWidthOptions(initialWidth = 2, maximumWidth = None)
  )

  testsFor {
    encoding(
      "variable-width codes",
      optionsWithVariableWidthCodes,
      inputSymbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X),
      /*
        Input | Output | Dict         | Code width
        ------+--------+--------------+----------
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
       */
      expectedBits = Seq(
        "00",
        "01",
        "10",
        "100",
        "011",
        "110",
        "101",
        "0100",
      ),
    )
  }

  testsFor(
    encoding(
      "variable-width codes with early change",
      optionsWithVariableWidthCodes.copy(
        codeWidth = optionsWithVariableWidthCodes.codeWidth.copy(
          earlyChange = true
        )
      ),
      inputSymbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X),
      /*
        Input | Output | Dict         | Code width
        ------+--------+--------------+----------
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
       */
      expectedBits = Seq(
        "00",
        "01",
        "010",
        "100",
        "011",
        "110",
        "0101",
        "0100",
      ),
    )
  )

  testsFor(
    encodingSteps(
      "variable-width codes with max width",
      optionsWithVariableWidthCodes.copy(
        codeWidth = optionsWithVariableWidthCodes.codeWidth.copy(
          maximumWidth = Some(3)
        )
      ),
    )(
      /*
        Input | Output | Dict         | Code width
        ------+--------+--------------+----------
              |        | b0: X        |
              |        | b1: o        | 2
        X     | b00    | b10: Xo      |
        o     | b01    | b11: oX      |
        Xo    | b10    | b100: XoX    | 3
        XoX   | b100   | b101: XoXo   |
        oX    | b011   | b110: oXo    |
        oXo   | b110   | b111: oXoX   |
        XoXo  | b101   | *AT MAX*
        XoXo  | b101   |
        XoXo  | b101   |
       */
      Encode(
        Seq(X, o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("00", "01", "10", "100", "011"),
        assertions = Seq((_.maxCodeWidthExhausted, false), (_.statistics.dictionarySize, 7)),
      ),
      Encode(
        Seq(X),
        expectedBits = Seq("110"),
        assertions = Seq((_.maxCodeWidthExhausted, /*CHANGED*/true), (_.statistics.dictionarySize, 8)),
      ),
      Encode(
        Seq(o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("101", "101"),
        assertions = Seq((_.maxCodeWidthExhausted, true), (_.statistics.dictionarySize, 8)),
      ),
      Finish(
        expectedBits = Seq("101"),
        assertions = Seq((_.maxCodeWidthExhausted, true), (_.statistics.dictionarySize, 8)),
      )
    )
  )

  testsFor(
    encodingSteps(
      "variable-width codes with max width and early change",
      optionsWithVariableWidthCodes.copy(
        codeWidth = optionsWithVariableWidthCodes.codeWidth.copy(
          maximumWidth = Some(3),
          earlyChange = true,
        )
      ),
    )(
      /*
        Input | Output | Dict         | Code width
        ------+--------+--------------+----------
              |        | b0: X        |
              |        | b1: o        | 2
        X     | b00    | b10: Xo      |
        o     | b01    | b11: oX      | 3
        Xo    | b010   | b100: XoX    |
        XoX   | b100   | b101: XoXo   |
        oX    | b011   | b110: oXo    |
        oXo   | b110   | b111: oXoX   |
        XoXo  | b101   | *AT MAX*
        XoXo  | b101   |
        XoXo  | b101   |
       */
      Encode(
        Seq(X, o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("00", "01", "010", "100", "011"),
        assertions = Seq((_.maxCodeWidthExhausted, false), (_.statistics.dictionarySize, 7)),
      ),
      Encode(
        Seq(X),
        expectedBits = Seq("110"),
        assertions = Seq((_.maxCodeWidthExhausted, /*CHANGED*/true), (_.statistics.dictionarySize, 8)),
      ),
      Encode(
        Seq(o, X, o, X, o, X, o, X, o, X, o),
        expectedBits = Seq("101", "101"),
        assertions = Seq((_.maxCodeWidthExhausted, true), (_.statistics.dictionarySize, 8)),
      ),
      Finish(
        expectedBits = Seq("101"),
        assertions = Seq((_.maxCodeWidthExhausted, true), (_.statistics.dictionarySize, 8)),
      ),
    )
  )

  testsFor(
    encodingSteps(
      "max dictionary size",
      Options(
        alphabet = Seq(X, o),
        codeWidth = CodeWidthOptions(initialWidth = 2, maximumWidth = None),
        maxDictionarySize = Some(5),
      ),
    )(
      /*
        Input | Output | Dict         | Code width
        ------+--------+--------------+----------
              |        | b0: X        |
              |        | b1: o        | 2
        X     | b00    | b10: Xo      |
        o     | b01    | b11: oX      |
        Xo    | b10    | b100: XoX    | 3
        XoX   | b100   | *AT MAX*
        oX    | b011   |
        oX    | b011   |
        oX    | b011   |
        oX    | b011   |
        oX    | b011   |
        oX    | b011   |
       */
      Encode(Seq(X, o, X, o),          expectedBits = Seq("00", "01"),          assertions = Seq((_.statistics.dictionarySize, 4))),

      Encode(Seq(X),                   expectedBits = Seq("10"),                assertions = Seq((_.statistics.dictionarySize, 5))),

      Encode(Seq(o, X, o, X, o, X, o), expectedBits = Seq("100", "011", "011"), assertions = Seq((_.statistics.dictionarySize, 5))),
      Encode(Seq(X, o, X, o, X, o, X), expectedBits = Seq("011", "011", "011"), assertions = Seq((_.statistics.dictionarySize, 5))),
      Finish(                          expectedBits = Seq("011"),               assertions = Seq((_.statistics.dictionarySize, 5))),
    )
  )

  testsFor(
    encodingSteps(
      "clear code",
      Options(
        alphabet = Seq(X, o),
        codeWidth = CodeWidthOptions(initialWidth = 2, maximumWidth = None),
        clearCode = Some(1),
      ),
    )(
      /*
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
       */
      Encode(Seq('X', 'o', 'X', 'o', 'X', 'o', 'X'), expectedBits = Seq("00", "10", "011"), assertions = Seq((_.statistics.dictionarySize, 5))),

      Reset(                                         expectedBits = Seq("101", "001"),      assertions = Seq((_.statistics.dictionarySize, 2))),

      Encode(Seq('o', 'X', 'o', 'X'),                expectedBits = Seq("10", "00"),        assertions = Seq((_.statistics.dictionarySize, 4))),
      Finish(                                        expectedBits = Seq("011"),             assertions = Seq((_.statistics.dictionarySize, 4))),
    )
  )

  testsFor(
    encoding(
      "stop code",
      Options(
        alphabet = Seq(X, o),
        codeWidth = CodeWidthOptions(initialWidth = 4, maximumWidth = None),
        stopCode = Some(1),
      ),
      inputSymbols = Seq(X, o, X, o, X, o, X),
      /*
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
       */
      expectedBits = Seq(
        "0000",
        "0010",
        "0011",
        "0101",
        "0001",
      ),
    )
  )

  testsFor(
    encodingSteps(
      "clear code and stop code",
      Options(
        alphabet = Seq(X, o),
        codeWidth = CodeWidthOptions.fixedWidth(4),
        clearCode = Some(3),
        stopCode = Some(4),
      ),
    )(
      /*
        Matched | Input | Output | Dictionary
        --------+-------+--------+-----------
                |       |        | b0: X
                |       |        | b1: o
                |       |        | b10: -
                |       |        | b11: CLEAR
                |       |        | b100: STOP
                | X     |        |
        X       | o     | b0000  | b101: Xo
        o       | X     | b0001  | b110: oX
        X       | o     |        |
        Xo      | X     | b0101  | b111: XoX
        X       | CLEAR | b0000  |
                |       | b0011  |-----------
                |       |        | b0: X
                |       |        | b1: o
                |       |        | b10: -
                |       |        | b11: CLEAR
                |       |        | b100: STOP
                | o     |        |
        o       | X     | b0001  | b101: oX
        X       | STOP  | b0000  |
                |       | b0100  |
       */
      Encode(Seq(X, o, X, o, X), expectedBits = Seq("0000", "0001", "0101"), assertions = Seq((_.statistics.dictionarySize, 5))),

      Reset(                     expectedBits = Seq("0000", "0011"),         assertions = Seq((_.statistics.dictionarySize, 2))),

      Encode(Seq(o, X),          expectedBits = Seq("0001"),                 assertions = Seq((_.statistics.dictionarySize, 3))),
      Finish(                    expectedBits = Seq("0000", "0100"),         assertions = Seq((_.statistics.dictionarySize, 3))),
    )
  )

  private def encoding[Sym](testName: String, options: Options[Sym], inputSymbols: Seq[Sym], expectedBits: Seq[String]): Unit =
    test(testName) {
      val encoder = new LzwEncoder(options)

      val blocks = Iterator.from(1)
        .scanLeft((Seq.empty[Sym], inputSymbols)) { case ((_, remainingSymbols), n) =>
          remainingSymbols.splitAt(n)
        }
        .drop(1)
        .map(_._1)
        .takeWhile(_.nonEmpty)
        .toSeq

      val init = blocks.flatMap(encoder.encode)
      val last = encoder.finish()
      val outputBitStrings = init ++ last

      assert(outputBitStrings == expectedBits.map(BitString.parse))
    }

  private sealed trait Step[Sym] {
    val expectedBits: Seq[String]
    val assertions: Seq[(LzwEncoder[Sym] => Any, Any)]
  }

  private case class Encode[Sym](inputSymbols: Seq[Sym], expectedBits: Seq[String], assertions: Seq[(LzwEncoder[Sym] => Any, Any)]) extends Step[Sym]
  private case class Reset[Sym](expectedBits: Seq[String], assertions: Seq[(LzwEncoder[Sym] => Any, Any)]) extends Step[Sym]
  private case class Finish[Sym](expectedBits: Seq[String], assertions: Seq[(LzwEncoder[Sym] => Any, Any)]) extends Step[Sym]

  private def encodingSteps[Sym](testName: String, options: Options[Sym])(steps: Step[Sym]*): Unit =
    test(testName) {
      val encoder = new LzwEncoder(options)

      for ((step, i) <- steps.zipWithIndex)
        withClue(s"(step index $i)") {
          val output = step match {
            case Encode(inputSymbols, _, _) => encoder.encode(inputSymbols)
            case Reset(_, _) => encoder.reset()
            case Finish(_, _) => encoder.finish()
          }
          assert(output == step.expectedBits.map(BitString.parse))

          for (((value, expected), i) <- step.assertions.zipWithIndex)
            withClue(s"(assertion index $i)") {
              assert(value(encoder) == expected)
            }
        }
    }
}
