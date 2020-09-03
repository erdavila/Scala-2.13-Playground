package lzw.core

import lzw.bits.BitString
import lzw.core.fixtures.{Fixture, Fixtures}
import org.scalatest.funsuite.AnyFunSuite

class LzwDecoderTest extends AnyFunSuite {

  testsFor(decoding(Fixtures.Empty))
  testsFor(decoding(Fixtures.FixedWidthCodes))
  testsFor(decoding(Fixtures.VariableWidthCodes))

  private def decoding[Sym](fixture: Fixture[Sym]): Unit = {
    test(fixture.name) {
      val decoder = new LzwDecoder(fixture.options)

      val blocks = Iterator.from(1)
        .scanLeft((Seq.empty[BitString], fixture.codesBits.map(BitString.parse))) { case ((_, remainingCodes), n) =>
          remainingCodes.splitAt(n)
        }
        .drop(1)
        .map(_._1)
        .takeWhile(_.nonEmpty)
        .toSeq

      val outputSymbols = blocks.flatMap(decoder.decode)

      assert(outputSymbols == fixture.symbols)
      assert(decoder.statistics.dictionarySize == fixture.dictionarySizeAtTheEnd)
    }
  }
}
