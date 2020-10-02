package lzw.bytes

import lzw.TestUtils
import lzw.bits.BitSignificance
import lzw.bytes.fixtures.{Fixture, Fixtures}
import org.scalatest.funspec.AnyFunSpec

class LzwByteEncoderTest extends AnyFunSpec {

  it should behave like encoding(Fixtures.Empty)
  it should behave like encoding(Fixtures.FixedWidthCodes)

  private def encoding(fixture: Fixture): Unit =
    describe(fixture.name) {
      val packingOrderCases = Seq(
        BitSignificance.LSB -> fixture.encodedBytesLsb,
        BitSignificance.MSB -> fixture.encodedBytesMsb,
      )

      val blocks = TestUtils.splitInIncreasingSizeGroups(fixture.decodedBytes.toIndexedSeq).toArray

      for ((packingOrder, expectedOutput) <- packingOrderCases) {
        it (s"encodes with $packingOrder packing order") {
          val encoder = new LzwByteEncoder(fixture.options.copy(packingOrder = packingOrder))

          val init = blocks.flatMap(a => encoder.encode(a.toArray))
          val last = encoder.finish()
          val outputBytes = init ++ last

          assert(outputBytes.toSeq == expectedOutput.toSeq)
        }
      }
    }
}
