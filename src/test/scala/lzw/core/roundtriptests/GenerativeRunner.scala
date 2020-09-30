package lzw.core.roundtriptests

import java.util.{Timer, TimerTask}
import lzw.core._
import scala.annotation.tailrec
import scala.concurrent.duration._

object GenerativeRunner {

  private val InputSizes = 0 until 15
  private def alphabetSizes(inputSize: Int): Range = 1 until (inputSize + 4)

  def main(args: Array[String]): Unit = {
    val threadCount = math.max(Runtime.getRuntime.availableProcessors() - 2, 1)
    val taskRunner = new TaskRunner(threadCount)

    val progress = new Progress(taskRunner.processedTasksCount)
    try {
      for (((options, inputSymbolsParts), index) <- casesIterator.zipWithIndex) {
        val inputSize = inputSymbolsParts.view.map(_.size).sum
        progress.prefix = s"$inputSize->${InputSizes.last} ${options.alphabet.size}->${alphabetSizes(inputSize).last}"

        taskRunner.submit {
          withCase(options, inputSymbolsParts, index) {
            processCase(options, inputSymbolsParts)
          }
        }
      }

      taskRunner.join()
    } finally {
      val finalStatus = progress.stop()
      println()
      println(finalStatus)
      println(s"Enqueue wait time (ns): ${taskRunner.enqueueWaitTime}")
      println(s"Dequeue wait time (ns): ${taskRunner.dequeueWaitTime}")
    }
  }

  private def processCase(options: Options[Sym], inputSymbolsParts: Seq[Seq[Sym]]): Unit = {
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

  private def casesIterator: Iterator[(Options[Sym], Seq[Seq[Sym]])] = {
    for {
      inputSize <- InputSizes.iterator
      alphabetSize <- alphabetSizes(inputSize).iterator
      inputSymbols <- inputSymbolsIterator(inputSize, alphabetSize)
      (clearCode, stopCode) <- specialCodesIterator(alphabetSize)
      inputSymbolsParts <- {
        val resetsAmounts = clearCode.fold(1)(_ => 2)
        for (resets <- Iterator.range(0, resetsAmounts))
          yield split(inputSymbols, resets + 1)
      }
      variableWidth <- Iterator(false, true)
      earlyChange <- Iterator(false, true)
      initialWidth <- {
        val minInitialWidth = Options.minInitialCodeWidth(alphabetSize, clearCode, stopCode, variableWidth, earlyChange)
        Iterator.range(0, 3).map(minInitialWidth + _)
      }
      maxWidth <- if (variableWidth) {
        Iterator(None) ++ Iterator.range(1, 4).map(n => Some(initialWidth + n))
      } else {
        Iterator(Some(initialWidth))
      }
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

  private def withCase[T](options: Options[Sym], inputSymbolsParts: Seq[Seq[Sym]], index: Int)(fun: => T): T =
    try {
      fun
    } catch {
      case t: Throwable =>
        val message =
          s"""${t.getMessage}
            |  index = $index
            |  options = $options
            |  inputSymbolsParts = $inputSymbolsParts
            |""".stripMargin.stripLineEnd

        throw new Exception(message, t)
    }

  private class Progress(totalCases: => Long) {
    var prefix: String = ""

    private val begin = System.nanoTime()
    private val timer = new Timer
    timer.scheduleAtFixedRate(new TimerTask {
      override def run(): Unit = print(s"\r$prefix: $status")
    }, 0, 1.second.toMillis)

    def stop(): String = {
      timer.cancel()
      status
    }

    private def status: String = {
      val end = System.nanoTime()
      val durationInNanos = end - begin
      val cases = totalCases
      val durationInSeconds = durationInNanos / 1.second.toNanos
      val rate = 1.second.toNanos * cases / durationInNanos
      s"$cases cases ÷ $durationInSeconds s = $rate cases/s"
    }
  }
}
