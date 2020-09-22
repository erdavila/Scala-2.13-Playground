package lzw.core

import lzw.bits.BitString
import lzw.core.LzwEncoder.Statistics
import scala.annotation.tailrec
import scala.collection.mutable

class LzwEncoder[Sym](val options: Options[Sym]) {

  private val dictionary: mutable.Map[Seq[Sym], Code] = mutable.Map.empty

  private var codeProducer: CodeProducer = _

  private object currentMatch {
    var symbols: Vector[Sym] = _
    var code: Code = _
  }

  initialize()

  private def initialize(): Unit = {
    dictionary.clear()
    codeProducer = new CodeProducer(options.codeWidth, options.clearCode.toSeq ++ options.stopCode)
    for (symbol <- options.alphabet) {
      addToDict(Vector(symbol))
    }

    currentMatch.symbols = Vector.empty
    currentMatch.code = -1
  }

  def encode(symbols: Seq[Sym]): Seq[BitString] =
    matchSymbols(symbols, Vector.empty)

  @tailrec
  private def matchSymbols(symbols: Seq[Sym], output: Seq[BitString]): Seq[BitString] =
    symbols match {
      case symbol +: remainingSymbols =>
        val newOutputOption = matchSymbol(symbol)
        matchSymbols(remainingSymbols, output ++ newOutputOption)
      case _ => output
    }

  private def matchSymbol(symbol: Sym): Option[BitString] = {
    val tentativeMatchedSymbols = currentMatch.symbols :+ symbol
    dictionary.get(tentativeMatchedSymbols) match {
      case Some(code) =>
        currentMatch.symbols = tentativeMatchedSymbols
        currentMatch.code = code
        None
      case None =>
        assert(tentativeMatchedSymbols.sizeIs >= 2)
        assert(currentMatch.symbols.nonEmpty)

        val newOutput = codeProducer.toBitString(currentMatch.code)

        addToDict(tentativeMatchedSymbols)

        currentMatch.symbols = Vector(symbol)
        currentMatch.code = dictionary(currentMatch.symbols)

        Some(newOutput)
    }
  }

  private def addToDict(symbols: Vector[Sym]): Unit =
    if (options.maxDictionarySize.forall(dictionary.size < _)) {
      for (code <- codeProducer.nextCode) {
        dictionary.put(symbols, code)
      }
    }

  def finish(): Seq[BitString] =
    flush().toSeq ++ options.stopCode.map(codeProducer.toBitString)

  def reset(): Seq[BitString] =
    options.clearCode match {
      case Some(code) =>
        val remaining = flush()
        val clearCode = codeProducer.toBitString(code)
        initialize()
        remaining.toSeq :+ clearCode
      case None => throw new UnsupportedOperationException("reset() is not supported because clear code is unset")
    }

  private def flush(): Option[BitString] =
    Option.when(currentMatch.symbols.nonEmpty) {
      codeProducer.toBitString(currentMatch.code)
    }

  def statistics: Statistics = Statistics(dictionary.size)
  def maxCodeWidthExhausted: Boolean = codeProducer.maxWidthExhausted
}

object LzwEncoder {
  case class Statistics(/*inputSymbols: Int, outputBits: Int, outputCodes: Int,*/ dictionarySize: Int)
}
