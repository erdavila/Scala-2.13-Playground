package lzw.bytes

import lzw.TestUtils
import lzw.bits.BitSignificance
import lzw.bytes.fixtures.{Fixture, Fixtures}
import org.scalatest.funspec.AnyFunSpec

class LzwByteDecoderTest extends AnyFunSpec {

  it should behave like decoding(Fixtures.Empty)
  it should behave like decoding(Fixtures.FixedWidthCodes)

  private def decoding(fixture: Fixture): Unit =
    describe(fixture.name) {
      val packingOrderCases = Seq(
        BitSignificance.LSB -> fixture.encodedBytesLsb,
        BitSignificance.MSB -> fixture.encodedBytesMsb,
      )

      for ((packingOrder, input) <- packingOrderCases) {
        it (s"decodes with $packingOrder packing order") {
          val decoder = new LzwByteDecoder(fixture.options.copy(packingOrder = packingOrder))
          val blocks = TestUtils.splitInIncreasingSizeGroups(input.toSeq)

          val outputBytes = blocks.flatMap(a => decoder.decode(a.toArray)).toSeq

          assert(outputBytes == fixture.decodedBytes.toSeq)
        }
      }
    }
}
