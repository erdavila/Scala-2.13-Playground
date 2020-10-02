package lzw.core

import lzw.TestUtils
import lzw.bits.BitString
import lzw.core.fixtures.SteppedEncodingFixture.{Encode, Finish, Reset, Step}
import lzw.core.fixtures.{Fixture, Fixtures, SteppedEncodingFixture}
import org.scalatest.funsuite.AnyFunSuite

class LzwEncoderTest extends AnyFunSuite {

  testsFor(encoding(Fixtures.Empty))
  testsFor(encoding(Fixtures.FixedWidthCodes))
  testsFor(encoding(Fixtures.VariableWidthCodes))
  testsFor(encoding(Fixtures.VariableWidthCodesWithEarlyChange))
  testsFor(encoding(Fixtures.VariableWidthCodesWithMaxWidth))
  testsFor(encoding(Fixtures.VariableWidthCodesWithMaxWidthAndEarlyChange))
  testsFor(encoding(Fixtures.MaxDictionarySize))
  testsFor(encoding(Fixtures.ClearCode))
  testsFor(encoding(Fixtures.StopCode))
  testsFor(encoding(Fixtures.ClearCodeAndStopCode))

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

      val blocks = TestUtils.splitInIncreasingSizeGroups(inputSymbols)

      val init = blocks.flatMap(encoder.encode).toSeq
      val last = encoder.finish()
      val outputBitStrings = init ++ last

      assert(outputBitStrings == expectedBits.map(BitString.parse))
      assert(encoder.statistics.inputSymbols == inputSymbols.size)
      assert(encoder.statistics.outputBits == outputBitStrings.view.map(_.length).sum)
      assert(encoder.statistics.outputCodes == outputBitStrings.size)
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
