package lzw.core

import lzw.utils.IterativeCasesUtils.{specialCodesIterator, split, withCase}
import lzw.utils.{ProgressDisplay, TaskRunner}
import scala.annotation.tailrec

object GeneratedRoundTripTestsRunner {
  private type Sym = Int

  private val InputSizes = 0 until 15
  private def alphabetSizes(inputSize: Int): Range = 1 until (inputSize + 4)

  def main(args: Array[String]): Unit = {
    case class Opts(repeat: Int, limit: Int)

    @tailrec
    def processArgs(args: List[String], opts: Opts): Opts =
      args match {
        case "--repeat" :: value :: rest => processArgs(rest, opts.copy(repeat = value.toInt))
        case "--limit" :: value :: rest => processArgs(rest, opts.copy(limit = value.toInt))
        case opt :: _ => throw new IllegalArgumentException(s"Unknown option or option missing parameter: $opt")
        case Nil => opts
      }

    val opts = processArgs(args.toList, Opts(repeat = 1, limit = Int.MaxValue))
    run(opts.repeat, opts.limit)
  }

  def run(repeat: Int, limit: Int): Unit = for (_ <- 1 to repeat) {
    val threadCount = math.max(Runtime.getRuntime.availableProcessors() - 2, 1)
    val taskRunner = new TaskRunner(threadCount)

    var progressPrefix: String = ""
    val progress = new ProgressDisplay(taskRunner.processedTasksCount, Some(() => progressPrefix), None)
    try {
      for (((options, inputSymbolsParts), index) <- casesIterator.take(limit).zipWithIndex) {
        val inputSize = inputSymbolsParts.view.map(_.size).sum
        progressPrefix = s"$inputSize->${InputSizes.last} ${options.alphabet.size}->${alphabetSizes(inputSize).last}"

        taskRunner.submit {
          withCase(index, options, "inputSymbolsParts" -> inputSymbolsParts) {
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

  private def casesIterator: Iterator[(Options[Sym], Seq[Seq[Sym]])] =
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
}
