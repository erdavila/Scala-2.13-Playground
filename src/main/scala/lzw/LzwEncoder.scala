package lzw

import lzw.LzwEncoder.Statistics
import scala.annotation.tailrec
import scala.collection.mutable

class LzwEncoder[Sym](val config: Config[Sym]) {
  type Code = Int

  private val dictionary: mutable.Map[Seq[Sym], Code] = mutable.Map(
    config.alphabet.zipWithIndex.map { case (symbol, code) =>
      Seq(symbol) -> code
    } : _*
  )

  private val codeWidth = config.codeConfig match {
    case CodeConfig.FixedWidth(width) => width
    case CodeConfig.VariableWidth(_, _, _) => ???
  }

  private var nextCode: Code = config.alphabet.length

  private object currentMatch {
    var symbols: Vector[Sym] = Vector.empty
    var code: Code = -1
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

            dictionary.put(tentativeMatchedSymbols, nextCode)
            nextCode += 1

            val newOutput = outputCode(currentMatch.code)

            currentMatch.symbols = Vector(symbol)
            currentMatch.code = dictionary(currentMatch.symbols)

            matchSymbols(remainingSymbols, output :+ newOutput)
        }
      case _ => output
    }

  def finish(): Option[BitString] =
    Option.when(currentMatch.symbols.nonEmpty) {
      outputCode(currentMatch.code)
    }

  private def outputCode(code: Code): BitString =
    BitString.from(code).lsb.take(codeWidth)

  def resetDictionary(): Array[Byte] = ???
  def statistics: Statistics = ???
}

object LzwEncoder {
  case class Statistics(inputBytes: Int, outputBytes: Int, dictionarySize: Int)
}
