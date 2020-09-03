package lzw.core

import java.nio.{ByteBuffer, ByteOrder}
import lzw.bits.BitString
import lzw.core.LzwDecoder.Statistics
import scala.annotation.tailrec
import scala.collection.mutable

class LzwDecoder[Sym](val options: Options[Sym]) {

  private val dictionary: mutable.Map[Code, Seq[Sym]] = mutable.Map(
    options.alphabet.zipWithIndex.map { case (sym, code) => code -> Seq(sym) } : _*
  )

  private var width = options.codeWidth.initialWidth
  private var widthIncreaseCode: Code = 1 << width
  private var nextCode: Code = options.alphabet.size
  private var lastOutputOption: Option[Seq[Sym]] = None

  def decode(codesBitStrings: Seq[BitString]): Seq[Sym] = {
    require(codesBitStrings.forall(_.length > 0))
    matchCodes(codesBitStrings, Seq.empty)
  }

  @tailrec
  private def matchCodes(codesBitStrings: Seq[BitString], output: Seq[Sym]): Seq[Sym] =
    codesBitStrings match {
      case codeBitString +: remainingCodesBitStrings =>
        require(codeBitString.length == width)
        val code = toCode(codeBitString)
        val newOutput = dictionary.get(code) match {
          case Some(newOutput) =>
            for (lastOutput <- lastOutputOption) {
              addToDictionary(lastOutput :+ newOutput.head)
            }

            newOutput
          case None =>
            assert(code == nextCode)
            val lastOutput = lastOutputOption.get
            val newOutput = lastOutput :+ lastOutput.head

            addToDictionary(newOutput)

            newOutput
        }

        lastOutputOption = Some(newOutput)
        matchCodes(remainingCodesBitStrings, output ++ newOutput)
      case _ => output
    }

  private def toCode(bitString: BitString): Code = {
    val bytes = bitString.lsb.bytes
      .padTo(java.lang.Integer.BYTES, 0.toByte)
      .toArray

    ByteBuffer.wrap(bytes)
      .order(ByteOrder.LITTLE_ENDIAN)
      .getInt(0)
  }

  private def addToDictionary(symbols: Seq[Sym]): Unit =
    dictionary.put(getNextCode, symbols)

  private def getNextCode: Code = {
    val value = nextCode
    nextCode += 1

    if (nextCode == widthIncreaseCode) {
      width += 1
      widthIncreaseCode <<= 1
    }

    value
  }

  def statistics: Statistics = Statistics(dictionary.size)
}

object LzwDecoder {
  case class Statistics(/*inputBits: Int, inputCodes: Int, outputSymbols: Int, */dictionarySize: Int)
}
