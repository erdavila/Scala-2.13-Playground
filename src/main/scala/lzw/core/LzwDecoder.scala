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
  private var _nextCode: Code = _
  private var lastOutputOption: Option[Seq[Sym]] = _
  private var _stopped: Boolean = false

  private object stats {
    var inputBits: Int = 0
    var inputCodes: Int = 0
    var outputSymbols: Int = 0

    def count(codes: Seq[BitString], symbols: Seq[Sym]): Unit = {
      inputBits += codes.view.map(_.length).sum
      inputCodes += codes.size
      outputSymbols += symbols.size
    }
  }

  initialize()

  private def initialize(): Unit = {
    _nextCode = 0
    skipReservedCodes()
    dictionary.clear()
    width = options.codeWidth.initialWidth
    widthIncreaseCode = 1 << width
    isMaxCodeWidthExhausted = false
    for (symbol <- options.alphabet) {
      addToDictionary(Vector(symbol))
    }
    lastOutputOption = None
  }

  def decode(codesBitStrings: Seq[BitString]): Seq[Sym] = {
    require(codesBitStrings.forall(_.length > 0))
    val symbols = matchCodes(codesBitStrings, Vector.empty)
    stats.count(codesBitStrings, symbols)
    symbols
  }

  def decode(codeBitString: BitString): Seq[Sym] = {
    val symbols = matchCode(codeBitString)
    stats.count(Seq(codeBitString), symbols)
    symbols
  }

  @tailrec
  private def matchCodes(codesBitStrings: Seq[BitString], output: Seq[Sym]): Seq[Sym] =
    codesBitStrings match {
      case _ +: _ if _stopped =>
        throw ExceedingCodesException(output, codesBitStrings.size)
      case codeBitString +: remainingCodesBitStrings =>
        val newOutput = matchCode(codeBitString)
        matchCodes(remainingCodesBitStrings, output ++ newOutput)
      case _ => output
    }

  private def matchCode(codeBitString: BitString): Seq[Sym] = {
    require(codeBitString.length == width, s"expected code with length $width instead of ${codeBitString.length}")
    val code = toCode(codeBitString)
    dictionary.get(code) match {
      case Some(newOutput) =>
        for (lastOutput <- lastOutputOption) {
          addToDictionary(lastOutput :+ newOutput.head)
        }

        lastOutputOption = Some(newOutput)
        newOutput

      case None if options.clearCode.contains(code) =>
        initialize()
        Vector.empty

      case None if options.stopCode.contains(code) =>
        _stopped = true
        Vector.empty

      case None =>
        assert(code == _nextCode)
        val lastOutput = lastOutputOption.get
        val newOutput = lastOutput :+ lastOutput.head

        addToDictionary(newOutput)

        lastOutputOption = Some(newOutput)
        newOutput
    }
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
      for (code <- nextCode) {
        dictionary.put(code, symbols)
      }
    }

  private def nextCode: Option[Code] =
    Option.when(!isMaxCodeWidthExhausted) {
      val value = _nextCode
      prepareNextCode()

      val delta = if (options.codeWidth.earlyChange) 1 else 0
      if (_nextCode + delta >= widthIncreaseCode
        && options.codeWidth.maximumWidth.forall(width < _)
        && options.maxDictionarySize.forall(dictionary.size + 1 < _)
      ) {
        width += 1
        widthIncreaseCode <<= 1
      }

      if (_nextCode >= widthIncreaseCode) {
        isMaxCodeWidthExhausted = true
      }

      value
    }

  private def prepareNextCode(): Unit = {
    _nextCode += 1
    skipReservedCodes()
  }

  private def skipReservedCodes(): Unit =
    while (options.clearCode.contains(_nextCode) || options.stopCode.contains(_nextCode)) {
      _nextCode += 1
    }

  def expectedCodeWidth: Int = width

  def stopped: Boolean = _stopped

  def statistics: Statistics = Statistics(
    stats.inputBits,
    stats.inputCodes,
    stats.outputSymbols,
    dictionary.size
  )
}

object LzwDecoder {
  case class Statistics(inputBits: Int, inputCodes: Int, outputSymbols: Int, dictionarySize: Int)

  case class ExceedingCodesException[Sym](lastSymbols: Seq[Sym], exceedingCodesCount: Int) extends Exception
}
