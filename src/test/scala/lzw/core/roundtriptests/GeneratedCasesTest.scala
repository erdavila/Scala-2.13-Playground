package lzw.core.roundtriptests

import lzw.bits.BitString
import lzw.core.{CodeWidthOptions, LzwDecoder, LzwEncoder, Options}
import org.scalatest.funspec.AnyFunSpec

class GeneratedCasesTest extends AnyFunSpec {
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
