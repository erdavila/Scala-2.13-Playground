package lzw.core.roundtriptests

import lzw.core._
import scala.annotation.tailrec

object GenerativeRunner {
  def main(args: Array[String]): Unit =
    try {
      for (((options, inputSymbolsParts), i) <- casesIterator.zipWithIndex) {
        withCase(i, options, inputSymbolsParts) {
          timing {
            val encoder = new LzwEncoder(options)
            val unfinishedOutputCodes = inputSymbolsParts.zipWithIndex
              .map { case (symbols, i) =>
                val flushedCodes = if (i > 0) encoder.reset() else Seq.empty
                flushedCodes ++ encoder.encode(symbols)
              }
              .reduce(_ ++ _)
            val outputCodes = unfinishedOutputCodes ++ encoder.finish()

            val decoder = new LzwDecoder(options)
            val decodedSymbols = decoder.decode(outputCodes)
            val inputSymbols = inputSymbolsParts.reduce(_ ++ _)
            assert(decodedSymbols == inputSymbols, s"$decodedSymbols != $inputSymbols [$outputCodes]")
          }
        }
      }
    } finally {
      timing.show()
    }

  private def casesIterator: Iterator[(Options[Sym], Seq[Seq[Sym]])] = {
    val InputSizes = 0 until 15
    for {
      inputSize <- InputSizes.iterator
      alphabetSizes = 2 until (inputSize + 10)
      alphabetSize <- alphabetSizes.iterator
      _ = print(s"\r$inputSize->${InputSizes.last} $alphabetSize->${alphabetSizes.last}")
      inputSymbols <- inputSymbolsIterator(inputSize, alphabetSize)
      (clearCode, stopCode) <- specialCodesIterator(alphabetSize)
      inputSymbolsParts <- {
        val resetsAmounts = clearCode.fold(1)(_ => 3)
        for (resets <- Iterator.range(0, resetsAmounts))
          yield split(inputSymbols, resets + 1)
      }
      initialWidth <- {
        val minInitialWidth = Options.minInitialCodeWidth(alphabetSize, clearCode, stopCode)
        Iterator.range(0, 4).map(minInitialWidth + _)
      }
      maxWidth <- Iterator(None) ++ Iterator.range(0, 5).map(n => Some(initialWidth + n))
      earlyChange <- Iterator(false, true)
      maxDictSize <- Iterator(None) ++ Iterator.range(alphabetSize, 2 * alphabetSize).map(Some(_))
      options = Options(
        alphabet = 0 until alphabetSize,
        codeWidth = CodeWidthOptions(
          initialWidth = initialWidth,
          maximumWidth = maxWidth,
          earlyChange = earlyChange,
        ),
        maxDictionarySize = maxDictSize,
        clearCode = clearCode,
        stopCode = stopCode,
      )
    } yield (options, inputSymbolsParts)
  }

  private def inputSymbolsIterator(inputSize: Int, alphabetSize: Int): Iterator[Seq[Sym]] = {
    @tailrec
    def increment(append: Vector[Sym])(symbols: Vector[Sym]): Option[Vector[Sym]] =
      if (symbols.isEmpty) {
        None
      } else if (symbols.last + 1 < alphabetSize) {
        Some(symbols.init ++ Seq(symbols.last + 1) ++ append)
      } else {
        increment(0 +: append)(symbols.init)
      }

    val start = Vector.fill(inputSize)(0)
    Iterator.iterate(Option(start))(_.flatMap(increment(Vector.empty)))
      .takeWhile(_.isDefined)
      .map(_.get)
  }

  private def specialCodesIterator(alphabetSize: Int): Iterator[(Option[Code], Option[Code])] = {
    def single: Iterator[Code] = Iterator(
      0,                // !ss⋯s__⋯
      1,                // s!s⋯s__⋯
      alphabetSize,     // ss⋯s!__⋯
      alphabetSize + 1, // ss⋯s_!_⋯
    )

    def both: Iterator[(Code, Code)] =
      Iterator(
        (0, 1),                               // !!sss⋯s___⋯
        (0, 2),                               // !s!ss⋯s___⋯
        (0, alphabetSize + 1),                // !sss⋯s!___⋯
        (0, alphabetSize + 2),                // !sss⋯s_!__⋯
        (1, 2),                               // s!!ss⋯s___⋯
        (1, 3),                               // s!s!s⋯s___⋯
        (1, alphabetSize + 1),                // s!ss⋯s!___⋯
        (1, alphabetSize + 2),                // s!ss⋯s_!__⋯
        (alphabetSize, alphabetSize + 1),     // sss⋯s!!___⋯
        (alphabetSize, alphabetSize + 2),     // sss⋯s!_!__⋯
        (alphabetSize + 1, alphabetSize + 2), // sss⋯s_!!__⋯
        (alphabetSize + 1, alphabetSize + 3), // sss⋯s_!_!_⋯
      )

    (
      Iterator((None, None)) ++
        (for (c <- single) yield (Some(c), None)) ++
        (for (c <- single) yield (None, Some(c))) ++
        (for ((c1, c2) <- both) yield (Some(c1), Some(c2))) ++
        (for ((c1, c2) <- both) yield (Some(c2), Some(c1)))
    ).distinct
  }

  private def split(symbols: Seq[Sym], parts: Int): Seq[Seq[Sym]] = {
    require(parts >= 1)
    if (parts == 1) {
      Seq(symbols)
    } else {
      val (part, rest) = symbols.splitAt(symbols.size / parts)
      part +: split(rest, parts - 1)
    }
  }

  def withCase[T](i: Int, options: Options[Sym], inputSymbolsParts: Seq[Seq[Sym]])(fun: => T): T =
    try {
      fun
    } catch {
      case t: Throwable =>
        val message =
          s"""${t.getMessage}
            |  index = $i
            |  options = $options
            |  inputSymbolsParts = $inputSymbolsParts
            |""".stripMargin.stripLineEnd

        throw new Exception(message, t)
    }

  private object timing {
    private sealed trait DoingWhat
    private case object Looping extends DoingWhat
    private case object Testing extends DoingWhat

    private var looping: Long = 0
    private var testing: Long = 0
    private var doingWhat: DoingWhat = _
    private var last: Long = _

    def apply[T](f: => T): T =
      try {
        transitionTo(Testing)
        f
      } finally {
        transitionTo(Looping)
      }

    private def transitionTo(what: DoingWhat): Unit = {
      val t = System.nanoTime()
      assert(what != doingWhat)

      if (doingWhat != null) {
        val diff = t - last
        doingWhat match {
          case Looping => looping += diff
          case Testing => testing += diff
        }
      }

      doingWhat = what
      last = System.nanoTime()
    }

    def show(): Unit = {
      def toSeconds(time: Long) = s"${time / 1000000000.0} s"
      println()
      println("Times")
      println(s"  testing: ${toSeconds(testing)}")
      println(s"  looping: ${toSeconds(looping)}")
      println(s"  total: ${toSeconds(testing + looping)}")
      println(s"  rate: ${testing.toDouble / looping}")
    }
  }
}
