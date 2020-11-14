package lzw.core

import lzw.bits.BitString
import lzw.core.LzwEncoder.Statistics
import scala.collection.mutable

class LzwEncoder[Sym](val options: Options[Sym]) {
  private val NoCode: Code = -1

  private val dictionary: mutable.Map[(Code, Sym), Code] = mutable.Map.empty
  private var codeProducer: CodeProducer = _
  private var currentMatchCode: Code = _

  private object stats {
    var inputSymbols: Int = 0
    var outputBits: Int = 0
    var outputCodes: Int = 0

    def count(symbols: Seq[Sym], codes: Seq[BitString]): Unit = {
      inputSymbols += symbols.size
      outputBits += codes.view.map(_.length).sum
      outputCodes += codes.size
    }
  }

  initialize()

  private def initialize(): Unit = {
    dictionary.clear()
    codeProducer = new CodeProducer(options.codeWidth, options.clearCode.toSeq ++ options.stopCode)
    for (symbol <- options.alphabet) {
      addToDict((NoCode, symbol))
    }

    currentMatchCode = NoCode
  }

  def encode(symbols: Seq[Sym]): Seq[BitString] = {
    val buffer = mutable.ArrayBuffer.empty[BitString]
    for (symbol <- symbols) {
      val codeOption = matchSymbol(symbol)
      buffer.appendAll(codeOption)
    }

    val codes = buffer.toVector
    stats.count(symbols, codes)
    codes
  }

  private def matchSymbol(symbol: Sym): Option[BitString] = {
    val tentativeCodeAndSymbol = (currentMatchCode, symbol)
    dictionary.get(tentativeCodeAndSymbol) match {
      case Some(code) =>
        currentMatchCode = code
        None
      case None =>
        assert(currentMatchCode != NoCode)

        val newOutput = codeProducer.toBitString(currentMatchCode)

        addToDict(tentativeCodeAndSymbol)

        currentMatchCode = dictionary((NoCode, symbol))

        Some(newOutput)
    }
  }

  private def addToDict(codeAndSymbol: (Code, Sym)): Unit =
    withNextCode { dictionary.put(codeAndSymbol, _) }

  def finish(): Seq[BitString] = {
    val flushed = flush().toSeq
    forceCodeWithEvaluation()

    val codes = flushed ++ options.stopCode.map(codeProducer.toBitString)
    stats.count(Seq.empty, codes)
    codes
  }

  private def withNextCode(f: Code => Unit): Unit =
    if (options.maxDictionarySize.forall(dictionary.size < _)) {
      for (code <- codeProducer.nextCode) {
        f(code)
      }
    }

  def reset(): Seq[BitString] =
    options.clearCode match {
      case Some(code) =>
        val remaining = flush()
        forceCodeWithEvaluation()
        val clearCode = codeProducer.toBitString(code)
        initialize()

        val codes = remaining.toSeq :+ clearCode
        stats.count(Seq.empty, codes)
        codes
      case None => throw new UnsupportedOperationException("reset() is not supported because clear code is unset")
    }

  private def flush(): Option[BitString] =
    Option.when(currentMatchCode != NoCode) {
      codeProducer.toBitString(currentMatchCode)
    }

  private def forceCodeWithEvaluation(): Unit =
    withNextCode { _ => }

  def statistics: Statistics = Statistics(
    stats.inputSymbols,
    stats.outputBits,
    stats.outputCodes,
    dictionary.size
  )

  def maxCodeWidthExhausted: Boolean = codeProducer.maxWidthExhausted
}

object LzwEncoder {
  case class Statistics(inputSymbols: Int, outputBits: Int, outputCodes: Int, dictionarySize: Int)
}
