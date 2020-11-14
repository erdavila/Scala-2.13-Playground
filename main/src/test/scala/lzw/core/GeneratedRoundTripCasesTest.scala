package lzw.core

import lzw.bits.BitString
import org.scalatest.funspec.AnyFunSpec

class GeneratedRoundTripCasesTest extends AnyFunSpec {
  it should behave like
    theCase(
      "finish()ing when width should increase",
      Options(
        alphabet = 0 until 2,
        codeWidth = CodeWidthOptions(
          initialWidth = 2,
          maximumWidth = None,
          earlyChange = false
        ),
        maxDictionarySize = None,
        clearCode = None,
        stopCode = Some(0)
      ),
      inputSymbolsParts = List(Vector(0, 0)),
      /*
        **************** ENCODING ****************
        Match | Input  | Output | Dict     | Width
        ------+--------+--------+----------+------
              |        |        | b0: STOP | 2
              |        |        | b1: 0    |
              |        |        | b10: 1   |
              | 0      |        |          |
        0     | 0      | b01    | b11: 0 0 |
        0     | finish | b01    | b100: *  | 3
              |        | b000   |          |

        ************ DECODING ************
        Input  | Output | Dict     | Width
        -------+--------+----------+------
               |        | b0: STOP | 2
               |        | b1: 0    |
               |        | b10: 1   |
        b01    | 0      |          |
        b01    | 0      | b11: 0 0 | 3
        b000   | STOP   |          |
       */
      outputCodesBits = Seq("01", "01", "000").map(BitString.parse)
    )

  it should behave like
    theCase(
      "maxWidthExhausted should be true on dictionary initialization",
      Options(
        alphabet = 0 until 2,
        codeWidth = CodeWidthOptions(
          initialWidth = 2,
          maximumWidth = Some(2),
          earlyChange = false
        ),
        maxDictionarySize = None,
        clearCode = Some(0),
        stopCode = Some(3)
      ),
      inputSymbolsParts = List(Vector(0, 0, 0)),
      /*
        ***************** ENCODING *****************
        Match | Input  | Output | Dict      | Width
        ------+--------+--------+-----------+------
              |        |        | b0: CLEAR | 2
              |        |        | b1: 0     |
              |        |        | b10: 1    |
              |        |        | b11: STOP |
              | 0      |        |           |
        0     | 0      | b01    | *AT MAX * |
        0     | 0      | b01    |           |
        0     | finish | b01    |           |
              |        | b11    |           |

        ************* DECODING *************
        Input  | Output | Dict      | Width
        -------+--------+-----------+------
               |        | b0: CLEAR | 2
               |        | b1: 0     |
               |        | b10: 1    |
               |        | b11: STOP |
        b01    | 0      |           |
        b01    | 0      |           |
        b01    | 0      |           |
        b11    | STOP   |           |
       */
      outputCodesBits = Seq("01", "01", "01", "11").map(BitString.parse)
    )

  it should behave like
    theCase(
      "width should not increase when dictionary is full",
      Options(
        alphabet = 0 until 2,
        codeWidth = CodeWidthOptions(
          initialWidth = 2,
          maximumWidth = None,
          earlyChange = true
        ),
        maxDictionarySize = Some(3),
        clearCode = None,
        stopCode = None
      ),
      inputSymbolsParts = List(Vector(0, 0, 1)),
      /*
        **************** ENCODING ****************
        Match | Input  | Output | Dict     | Width
        ------+--------+--------+----------+------
              |        |        | b0: 0    | 2
              |        |        | b1: 1    |
              | 0      |        |          |
        0     | 0      | b00    | b10: 0 0 |
        0     | 1      | b00    | *AT MAX* |
        1     | finish | b01

        ************* DECODING *************
        Input  | Output | Dict      | Width
        -------+--------+-----------+------
               |        | b0: 0     | 2
               |        | b1: 1     |
        b00    | 0      |           |
        b00    | 0      | b10: 0 0  |
        b01    | 1
       */
      outputCodesBits = Seq("00", "00", "01").map(BitString.parse)
    )

  it should behave like
    theCase(
      "reset()ing when width should increase",
      Options(
        alphabet = 0 until 2,
        codeWidth = CodeWidthOptions(
          initialWidth = 2,
          maximumWidth = None,
          earlyChange = false
        ),
        maxDictionarySize = None,
        clearCode = Some(0),
        stopCode = None
      ),
      inputSymbolsParts = List(Vector(0, 0), Vector(0, 0)),
      /*
        ***************** ENCODING *****************
        Match | Input  | Output | Dict      | Width
        ------+--------+--------+-----------+------
              |        |        | b0: CLEAR | 2
              |        |        | b1: 0     |
              |        |        | b10: 1    |
              | 0      |        |           |
        0     | 0      | b01    | b11: 0 0  |
        0     | reset  | b01    |           | 3
              |        | b000   |-----------|------
              |        |        | b0: CLEAR | 2
              |        |        | b1: 0     |
              |        |        | b10: 1    |
              | 0      |        |           |
        0     | 0      | b01    | b11: 0 0  |
        0     | finish | b01    |           |

        ************* DECODING *************
        Input  | Output | Dict      | Width
        -------+--------+-----------+------
               |        | b0: CLEAR | 2
               |        | b1: 0     |
               |        | b10: 1    |
        b01    | 0      |           |
        b01    | 0      | b11: 0 0  | 3
        b000   | CLEAR  |-----------|------
               |        | b0: CLEAR | 2
               |        | b1: 0     |
               |        | b10: 1    |
        b01    | 0      |           |
        b01    | 0      | b11: 0 0  | 3
       */
      outputCodesBits = Seq("01", "01", "000", "01", "01").map(BitString.parse)
    )

  private def theCase(
    name: String,
    options: Options[Int],
    inputSymbolsParts: Seq[Seq[Int]],
    outputCodesBits: Seq[BitString]
  ): Unit =
    describe(name) {
      it("encodes") {
        val encoder = new LzwEncoder(options)
        val unfinishedOutputCodes = inputSymbolsParts.zipWithIndex
          .map { case (symbols, i) =>
            val flushedCodes = if (i > 0) encoder.reset() else Seq.empty
            flushedCodes ++ encoder.encode(symbols)
          }
          .reduce(_ ++ _)
        val outputCodes = unfinishedOutputCodes ++ encoder.finish()
        assert(outputCodes == outputCodesBits)
      }

      it("decodes") {
        val decoder = new LzwDecoder(options)
        val decodedSymbols = decoder.decode(outputCodesBits)
        val inputSymbols = inputSymbolsParts.reduce(_ ++ _)
        assert(decodedSymbols == inputSymbols)
      }
    }
}
