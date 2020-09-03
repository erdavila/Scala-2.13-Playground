package lzw

import org.scalatest.funspec.AnyFunSpec

class LzwEncoderTest extends AnyFunSpec {
  private val bytesConfig = Config(
    alphabet = Seq.range(0, 256).map(_.toByte),
    codeConfig = CodeConfig.fixedWidth(12),
  )

  describe("empty input") {
    it should behave like encoding(bytesConfig, Seq.empty, Seq.empty)
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

    it should behave like encoding(bytesConfig, inputBytes, expectedBitStrings)
  }

  describe("variable-width codes") {
    val X = 'X'
    val o = 'o'

    val config = Config(
      alphabet = Seq(X, o),
      codeConfig = CodeConfig(initialWidth = 2, maximumWidth = None, earlyChange = false)
    )

    val inputSymbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X)

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

    val expectedBitStrings = Seq(
      BitString.parse("00"),
      BitString.parse("01"),
      BitString.parse("10"),
      BitString.parse("100"),
      BitString.parse("011"),
      BitString.parse("110"),
      BitString.parse("101"),
      BitString.parse("0100"),
    )

    encoding(config, inputSymbols, expectedBitStrings)
  }

  private def encoding[Sym](config: Config[Sym], inputSymbols: Seq[Sym], expectedBitStrings: Seq[BitString]): Unit =
    it("encodes") {
      val encoder = new LzwEncoder(config)

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

      assert(outputBitStrings == expectedBitStrings)
    }
}
