package lzw.core

import java.nio.{ByteBuffer, ByteOrder}
import lzw.bits.BitString
import lzw.core.LzwDecoder.{ExceedingCodesException, Statistics}
import scala.annotation.tailrec
import scala.collection.mutable

class LzwDecoder[Sym](val options: Options[Sym]) {

  private val dictionary: mutable.Map[Code, Seq[Sym]] = mutable.Map.empty
  private var width = -1
  private var widthIncreaseCode: Code = _
  private var isMaxCodeWidthExhausted: Boolean = _
  private var nextCode: Code = _
  private var lastOutputOption: Option[Seq[Sym]] = _
  private var _stopped: Boolean = false

  initialize()

  private def initialize(): Unit = {
    dictionary.clear()
    val specialCodes = options.clearCode ++ options.stopCode

    val codes = {
      val minCode = specialCodes.minOption
      val maxCode = specialCodes.maxOption

      val codesRangeA = Iterator.range(0, minCode.getOrElse(0))
      val codesRangeB = Iterator.range(minCode.fold(0)(_ + 1), maxCode.getOrElse(0))
      val codesRangeC = Iterator.from(maxCode.fold(0)(_ + 1))

      (codesRangeA ++ codesRangeB ++ codesRangeC).take(options.alphabet.size).toVector
    }
    dictionary.addAll(
      for ((sym, code) <- options.alphabet `zip` codes)
        yield code -> Vector(sym)
    )

    width = options.codeWidth.initialWidth
    widthIncreaseCode = 1 << width
    isMaxCodeWidthExhausted = false
    nextCode = (codes.last +: specialCodes.toSeq).max + 1
    lastOutputOption = None
  }

  def decode(codesBitStrings: Seq[BitString]): Seq[Sym] = {
    require(codesBitStrings.forall(_.length > 0))
    matchCodes(codesBitStrings, Seq.empty)
  }

  @tailrec
  private def matchCodes(codesBitStrings: Seq[BitString], output: Seq[Sym]): Seq[Sym] =
    codesBitStrings match {
      case _ +: _ if _stopped =>
        throw ExceedingCodesException(output, codesBitStrings.size)

      case codeBitString +: remainingCodesBitStrings =>
        require(codeBitString.length == width)
        val code = toCode(codeBitString)
        dictionary.get(code) match {
          case Some(newOutput) =>
            for (lastOutput <- lastOutputOption) {
              addToDictionary(lastOutput :+ newOutput.head)
            }

            lastOutputOption = Some(newOutput)
            matchCodes(remainingCodesBitStrings, output ++ newOutput)

          case None if options.clearCode.contains(code) =>
            initialize()
            matchCodes(remainingCodesBitStrings, output)

          case None if options.stopCode.contains(code) =>
            _stopped = true
            matchCodes(remainingCodesBitStrings, output)

          case None =>
            assert(code == nextCode)
            val lastOutput = lastOutputOption.get
            val newOutput = lastOutput :+ lastOutput.head

            addToDictionary(newOutput)

            lastOutputOption = Some(newOutput)
            matchCodes(remainingCodesBitStrings, output ++ newOutput)
        }
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
    if (options.maxDictionarySize.forall(dictionary.sizeIs < _)) {
      for (code <- getNextCode) {
        dictionary.put(code, symbols)
      }
    }

  private def getNextCode: Option[Code] =
    Option.when(!isMaxCodeWidthExhausted) {
      val value = nextCode
      nextCode += 1

      val delta = if (options.codeWidth.earlyChange) 1 else 0
      if (nextCode + delta == widthIncreaseCode && options.codeWidth.maximumWidth.forall(width < _)) {
        width += 1
        widthIncreaseCode <<= 1
      }

      if (nextCode == widthIncreaseCode) {
        isMaxCodeWidthExhausted = true
      }

      value
    }

  def stopped: Boolean = _stopped

  def statistics: Statistics = Statistics(dictionary.size)
}

object LzwDecoder {
  case class Statistics(/*inputBits: Int, inputCodes: Int, outputSymbols: Int, */dictionarySize: Int)

  case class ExceedingCodesException[Sym](lastSymbols: Seq[Sym], exceedingCodesCount: Int) extends Exception
}
