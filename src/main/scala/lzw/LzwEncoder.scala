package lzw

import lzw.LzwEncoder.Statistics

import scala.annotation.tailrec
import scala.collection.mutable

class LzwEncoder[Sym](val config: Config[Sym]) {

  private val dictionary: mutable.Map[Seq[Sym], Code] = mutable.Map.empty

  private var codeProducer: CodeProducer = _

  private object currentMatch {
    var symbols: Vector[Sym] = _
    var code: Code = _
  }

  initialize()

  private def initialize(): Unit = {
    dictionary.clear()

    @tailrec
    def addToDict(nextCode: Code, symbols: Seq[Sym]): Code =
      symbols match {
        case symbol +: rest =>
          if (config.clearCode.contains(nextCode)) {
            addToDict(nextCode + 1, symbols)
          } else {
            dictionary.put(Seq(symbol), nextCode)
            addToDict(nextCode + 1, rest)
          }
        case _ => nextCode
      }
    val nextCode = addToDict(0, config.alphabet)

    codeProducer = new CodeProducer(
      config.codeConfig,
      firstNextCode = math.max(nextCode, config.clearCode.fold(0)(_ + 1)),
    )

    currentMatch.symbols = Vector.empty
    currentMatch.code = -1
  }

  def encode(symbols: Seq[Sym]): Seq[BitString] =
    matchSymbols(symbols, Seq.empty)

  @tailrec
  private def matchSymbols(symbols: Seq[Sym], output: Seq[BitString]): Seq[BitString] =
    symbols match {
      case symbol +: remainingSymbols =>
        val tentativeMatchedSymbols = currentMatch.symbols :+ symbol
        dictionary.get(tentativeMatchedSymbols) match {
          case Some(code) =>
            currentMatch.symbols = tentativeMatchedSymbols
            currentMatch.code = code

            matchSymbols(remainingSymbols, output)
          case None =>
            assert(tentativeMatchedSymbols.sizeIs >= 2)
            assert(currentMatch.symbols.nonEmpty)

            val newOutput = codeProducer.toBitString(currentMatch.code)

            if (config.maxDictionarySize.forall(dictionary.size < _)) {
              for (code <- codeProducer.nextCode) {
                dictionary.put(tentativeMatchedSymbols, code)
              }
            }

            currentMatch.symbols = Vector(symbol)
            currentMatch.code = dictionary(currentMatch.symbols)

            matchSymbols(remainingSymbols, output :+ newOutput)
        }
      case _ => output
    }

  def finish(): Option[BitString] =
    Option.when(currentMatch.symbols.nonEmpty) {
      codeProducer.toBitString(currentMatch.code)
    }

  def reset(): Seq[BitString] =
    config.clearCode match {
      case Some(code) =>
        val remaining = finish()
        val clearCode = codeProducer.toBitString(code)
        initialize()
        remaining.toSeq :+ clearCode
      case None => throw new UnsupportedOperationException("reset() is not supported because clear code is unset")
    }

  def statistics: Statistics = Statistics(dictionary.size)
  def maxCodeWidthExhausted: Boolean = codeProducer.maxWidthExhausted
}

object LzwEncoder {
  case class Statistics(/*inputSymbols: Int, outputBits: Int, outputCodes: Int,*/ dictionarySize: Int)
}
