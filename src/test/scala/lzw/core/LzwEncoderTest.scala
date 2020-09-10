package lzw.core

import lzw.bits.BitString
import lzw.core.fixtures.SteppedEncodingFixture.{Encode, Finish, Reset, Step}
import lzw.core.fixtures.{Fixture, Fixtures, SteppedEncodingFixture}
import org.scalatest.funsuite.AnyFunSuite

class LzwEncoderTest extends AnyFunSuite {

  import Fixtures.{X, o}

  testsFor(encoding(Fixtures.Empty))
  testsFor(encoding(Fixtures.FixedWidthCodes))
  testsFor(encoding(Fixtures.VariableWidthCodes))
  testsFor(encoding(Fixtures.VariableWidthCodesWithEarlyChange))
  testsFor(encoding(Fixtures.VariableWidthCodesWithMaxWidth))
  testsFor(encoding(Fixtures.VariableWidthCodesWithMaxWidthAndEarlyChange))
  testsFor(encoding(Fixtures.MaxDictionarySize))
  testsFor(encoding(Fixtures.ClearCode))

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
      expectedDictionarySizeAtTheEnd = 5,
    )
  )

  testsFor(
    encoding(
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

  private def encoding[Sym](fixture: Fixture[Sym]): Unit =
    encoding(fixture.name, fixture.options, fixture.symbols, fixture.codesBits, fixture.dictionarySizeAtTheEnd)

  private def encoding[Sym](
    testName: String,
    options: Options[Sym],
    inputSymbols: Seq[Sym],
    expectedBits: Seq[String],
    expectedDictionarySizeAtTheEnd: Int,
  ): Unit =
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
      assert(encoder.statistics.dictionarySize == expectedDictionarySizeAtTheEnd)
    }

  private def encoding[Sym](fixture: SteppedEncodingFixture[Sym]): Unit =
    encoding(fixture.name, fixture.options)(fixture.steps: _*)

  private def encoding[Sym](testName: String, options: Options[Sym])(steps: Step[Sym]*): Unit =
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
