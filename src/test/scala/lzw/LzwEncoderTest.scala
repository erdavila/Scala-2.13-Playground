package lzw

import org.scalatest.funspec.AnyFunSpec

class LzwEncoderTest extends AnyFunSpec {
  private val config = Config(
    alphabet = Seq.range(0, 256).map(_.toByte),
    codeConfig = CodeConfig.FixedWidth(12),
  )

  describe("empty input") {
    it should behave like encoding(Seq.empty, Seq.empty)
  }

  describe("simple non-empty input") {
    val inputBytes = {
      val X = 'X'.toByte // b01011000
      val o = 'o'.toByte // b01101111
      Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X)
    }

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

    val expectedBitStrings = Seq(
      BitString.parse("000001011000"),
      BitString.parse("000001101111"),
      BitString.parse("000100000000"),
      BitString.parse("000100000010"),
      BitString.parse("000100000001"),
      BitString.parse("000100000100"),
      BitString.parse("000100000010"),
    )

    it should behave like encoding(inputBytes, expectedBitStrings)
  }

  private def encoding(inputBytes: Seq[Byte], expectedBitStrings: Seq[BitString]): Unit =
    it("encodes") {
      val encoder = new LzwEncoder(config)

      val blocks = Iterator.from(1)
        .scanLeft((Seq.empty[Byte], inputBytes)) { case ((_, remainingBytes), n) =>
          remainingBytes.splitAt(n)
        }
        .drop(1)
        .map(_._1)
        .takeWhile(_.nonEmpty)
        .toSeq

      val init = blocks.flatMap(encoder.encode)
      val last = encoder.finish()
      val outputBitStrings = init ++ last

      assert(outputBitStrings == expectedBitStrings)
    }
}
