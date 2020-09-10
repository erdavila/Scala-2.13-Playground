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
      codeConfig = CodeConfig(initialWidth = 2, maximumWidth = None)
    )

    val inputSymbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X)

    describe("basic case") {
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

    describe("with early change") {
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

      val expectedBitStrings = Seq(
        BitString.parse("00"),
        BitString.parse("01"),
        BitString.parse("010"),
        BitString.parse("100"),
        BitString.parse("011"),
        BitString.parse("110"),
        BitString.parse("0101"),
        BitString.parse("0100"),
      )

      val configWithEarlyChange = config.copy(
        codeConfig = config.codeConfig.copy(
          earlyChange = true
        )
      )
      encoding(configWithEarlyChange, inputSymbols, expectedBitStrings)
    }

    describe("with max dictionary size") {
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

      val expectedBitStrings = Seq(
        BitString.parse("00"),
        BitString.parse("01"),
        BitString.parse("10"),
        BitString.parse("100"),
        BitString.parse("011"),
        BitString.parse("011"),
        BitString.parse("011"),
        BitString.parse("011"),
        BitString.parse("011"),
        BitString.parse("011"),
      )

      val configWithMaxDictSize = config.copy(maxDictionarySize = Some(5))
      checkedEncodingSteps(
        configWithMaxDictSize,
        inputSymbols, 5,
        expectedBitStrings, 3,
      )(
        _.statistics.dictionarySize < 5,
        _.statistics.dictionarySize == 5,
      )
    }

    describe("with max code width") {
      val inputSymbols = Seq(X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o, X, o)

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

      val expectedBitStrings = Seq(
        BitString.parse("00"),
        BitString.parse("01"),
        BitString.parse("10"),
        BitString.parse("100"),
        BitString.parse("011"),
        BitString.parse("110"),
        BitString.parse("101"),
        BitString.parse("101"),
        BitString.parse("101"),
      )

      val configWithMaxCodeWidth = config.copy(
        codeConfig = config.codeConfig.copy(
          maximumWidth = Some(3)
        )
      )
      checkedEncodingSteps(
        configWithMaxCodeWidth,
        inputSymbols, 13,
        expectedBitStrings, 6,
      )(
        !_.maxCodeWidthExhausted,
        _.maxCodeWidthExhausted,
      )

      describe("and early change") {
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

        val expectedBitStrings = Seq(
          BitString.parse("00"),
          BitString.parse("01"),
          BitString.parse("010"),
          BitString.parse("100"),
          BitString.parse("011"),
          BitString.parse("110"),
          BitString.parse("101"),
          BitString.parse("101"),
          BitString.parse("101"),
        )

        val configWithMaxCodeWidthAndEarlyChange = config.copy(
          codeConfig = config.codeConfig.copy(
            maximumWidth = Some(3),
            earlyChange = true,
          )
        )
        checkedEncodingSteps(
          configWithMaxCodeWidthAndEarlyChange,
          inputSymbols, 13,
          expectedBitStrings, 6,
        )(
          !_.maxCodeWidthExhausted,
          _.maxCodeWidthExhausted,
        )
      }
    }
  }

  describe("clear code") {
    val X = 'X'
    val o = 'o'

    val config = Config(
      alphabet = Seq(X, o),
      codeConfig = CodeConfig(initialWidth = 2, maximumWidth = None),
      clearCode = Some(1),
    )

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

    it("encodes") {
      val encoder = new LzwEncoder(config)

      {
        val output = encoder.encode(Seq('X', 'o', 'X', 'o', 'X', 'o', 'X'))
        val expected = Seq("00", "10", "011").map(BitString.parse)
        assert(output == expected)
      }
      assert(encoder.statistics.dictionarySize == 5)

      assert {
        val output = encoder.reset()
        val expected = Seq("101", "001").map(BitString.parse)
        output == expected
      }
      assert(encoder.statistics.dictionarySize == 2)

      assert {
        val output = encoder.encode(Seq('o', 'X', 'o', 'X'))
        val expected = Seq("10", "00").map(BitString.parse)
        output == expected
      }
      assert(encoder.statistics.dictionarySize == 4)

      {
        val output = encoder.finish()
        val expected = Seq("011").map(BitString.parse)
        assert(output == expected)
      }
    }
  }

  describe("stop code") {
    val X = 'X'
    val o = 'o'

    val config = Config(
      alphabet = Seq(X, o),
      codeConfig = CodeConfig(
        initialWidth = 4,
        maximumWidth = None,
      ),
      stopCode = Some(1),
    )

    val inputSymbols = Seq(X, o, X, o, X, o, X)

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

    val expectedBitStrings = Seq(
      "0000",
      "0010",
      "0011",
      "0101",
      "0001",
    ).map(BitString.parse)

    encoding(config, inputSymbols, expectedBitStrings)
  }

  describe("clear code and stop code") {
    val X = 'X'
    val o = 'o'

    val config = Config(
      alphabet = Seq(X, o),
      codeConfig = CodeConfig.fixedWidth(4),
      clearCode = Some(3),
      stopCode = Some(4),
    )

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

    it("encodes") {
      val encoder = new LzwEncoder(config)

      {
        val output = encoder.encode(Seq(X, o, X, o, X))
        val expected = Seq("0000", "0001", "0101").map(BitString.parse)
        assert(output == expected)
      }
      assert(encoder.statistics.dictionarySize == 5)

      {
        val output = encoder.reset()
        val expected = Seq("0000", "0011").map(BitString.parse)
        assert(output == expected)
      }
      assert(encoder.statistics.dictionarySize == 2)

      {
        val output = encoder.encode(Seq(o, X))
        val expected = Seq("0001").map(BitString.parse)
        assert(output == expected)
      }
      assert(encoder.statistics.dictionarySize == 3)

      {
        val output = encoder.finish()
        val expected = Seq("0000", "0100").map(BitString.parse)
        assert(output == expected)
      }
    }
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

  private def checkedEncodingSteps[Sym](
    config: Config[Sym],
    inputSymbols: Seq[Sym], inputIndex: Int,
    expectedBitStrings: Seq[BitString], outputIndex: Int
  )(
    assertionBefore: LzwEncoder[Sym] => Unit,
    assertionAfter: LzwEncoder[Sym] => Unit,
  ): Unit =
    it("encodes") {
      val encoder = new LzwEncoder(config)

      def eachStep(input: Seq[(Sym, Int)], assertion: LzwEncoder[Sym] => Unit) =
        for {
          (symbol, i) <- input
          bitStrings = encoder.encode(Seq(symbol))
          _ = withClue(s"index $i") { assertion(encoder) }
          bitString <- bitStrings
        } yield bitString

      val (inputBefore, inputAfter) = inputSymbols.zipWithIndex.splitAt(inputIndex)
      val (expectedBitStringsBefore, expectedBitStringsAfter) = expectedBitStrings.splitAt(outputIndex)

      val outputBefore = eachStep(inputBefore, assertionBefore)
      assert(outputBefore == expectedBitStringsBefore)

      val outputAfter = eachStep(inputAfter, assertionAfter)
      assert(outputAfter ++ encoder.finish() == expectedBitStringsAfter)
    }
}
