package lzw.core

import lzw.bits.BitString
import lzw.core.fixtures.{Fixture, Fixtures}
import lzw.utils.TestUtils
import org.scalatest.funsuite.AnyFunSuite

class LzwDecoderTest extends AnyFunSuite {

  testsFor(decoding(Fixtures.Empty))
  testsFor(decoding(Fixtures.FixedWidthCodes))
  testsFor(decoding(Fixtures.VariableWidthCodes))
  testsFor(decoding(Fixtures.VariableWidthCodesWithEarlyChange))
  testsFor(decoding(Fixtures.VariableWidthCodesWithMaxWidth.toFixture))
  testsFor(decoding(Fixtures.VariableWidthCodesWithMaxWidthAndEarlyChange.toFixture))
  testsFor(decoding(Fixtures.MaxDictionarySize.toFixture))
  testsFor(decoding(Fixtures.ClearCode.toFixture))
  testsFor(decoding(Fixtures.StopCode))

  test("stop code sets stopped attribute") {
    val StopCodeFixture = Fixtures.StopCode
    val initCodes :+ lastCode = StopCodeFixture.codesBits.map(BitString.parse)

    val decoder = new LzwDecoder(StopCodeFixture.options)
    assert(!decoder.stopped)

    val outputBegin = decoder.decode(initCodes)
    assert(!decoder.stopped)

    val outputEnd = decoder.decode(Seq(lastCode))
    assert(decoder.stopped)

    assert(outputBegin ++ outputEnd == StopCodeFixture.symbols)
  }

  test("stop code with exceeding codes") {
    val StopCodeFixture = Fixtures.StopCode
    val ExceedingCodesBits = Seq("0000", "1111").map(BitString.parse)
    val initCodes :+ lastCode = StopCodeFixture.codesBits.map(BitString.parse)
    val decoder = new LzwDecoder(StopCodeFixture.options)

    val outputSymbols = decoder.decode(initCodes)
    val e = intercept[LzwDecoder.ExceedingCodesException[_]] {
      decoder.decode(lastCode +: ExceedingCodesBits)
    }

    assert(outputSymbols ++ e.lastSymbols == StopCodeFixture.symbols)
    assert(e.exceedingCodesCount == ExceedingCodesBits.size)
  }

  testsFor(decoding(Fixtures.ClearCodeAndStopCode.toFixture))

  private def decoding[Sym](fixture: Fixture[Sym]): Unit = {
    test(fixture.name) {
      val decoder = new LzwDecoder(fixture.options)

      val inputCodes = fixture.codesBits.map(BitString.parse)
      val blocks = TestUtils.splitInIncreasingSizeGroups(inputCodes)

      val outputSymbols = blocks.flatMap(decoder.decode).toSeq

      assert(outputSymbols == fixture.symbols.toSeq)
      assert(decoder.statistics.inputBits == inputCodes.view.map(_.length).sum)
      assert(decoder.statistics.inputCodes == inputCodes.size)
      assert(decoder.statistics.outputSymbols == outputSymbols.size)
      assert(decoder.statistics.dictionarySize == fixture.dictionarySizeAtTheEnd)
    }
  }
}
