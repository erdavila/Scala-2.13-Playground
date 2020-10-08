package lzw.bytes.roundtriptests

import java.util.zip.ZipInputStream
import lzw.bits.BitSignificance
import lzw.bytes.Options.BytesAlphabetSize
import lzw.bytes.{CodeWidthOptions, LzwByteDecoder, LzwByteEncoder, Options}
import lzw.utils.IterativeCasesUtils.{specialCodesIterator, split, withCase}
import lzw.utils.{ProgressDisplay, TaskRunner}
import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object VaryingOptionsRunner {

  private val MaxResets = 2

  def main(args: Array[String]): Unit = {
    val threadCount = math.max(Runtime.getRuntime.availableProcessors() - 2, 1)
    val taskRunner = new TaskRunner(threadCount)

    val contentByResets = getContentByResets
    val cases = casesIterator.toArray

    val progress = new ProgressDisplay(taskRunner.processedTasksCount)
    try {
      for (((options, resets), index) <- cases.zipWithIndex) {
        progress.prefix = s"$index->${cases.length}"

        taskRunner.submit {
          withCase(index, options, "resets" -> resets) {
            processCase(options, resets, contentByResets)
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

  private def processCase(options: Options, resets: Int, decodedBytesByResets: Array[Array[Array[Byte]]]): Unit = {
    val decodedBytesParts = decodedBytesByResets(resets)

    val encoder = new LzwByteEncoder(options)
    val unfinishedEncodedBytes = decodedBytesParts.zipWithIndex
      .map { case (bytes, i) =>
        /*val flushedEncodedBytes = if (i > 0) encoder.reset() else Seq.empty
        flushedEncodedBytes ++ */encoder.encode(bytes) // !!
      }
      .reduce(_ ++ _)
    val encodedBytes = unfinishedEncodedBytes ++ encoder.finish()

    val decoder = new LzwByteDecoder(options)
    val decodedBytes = decoder.decode(encodedBytes)
    val expectedDecodedBytes = decodedBytesByResets(0)(0)
    assert(decodedBytes `sameElements` expectedDecodedBytes)
  }

  private def casesIterator: Iterator[(Options, Int)] =
    for {
      options <- optionsIterator
      resets <- {
        val resetsAmounts = MaxResets//options.clearCode.fold(1)(_ => MaxResets) !!
        (0 to resetsAmounts).iterator.take(1) // !!
      }
    } yield (options, resets)

  private def optionsIterator: Iterator[Options] =
    for {
      (clearCode, stopCode) <- specialCodesIterator(BytesAlphabetSize).take(1) // !!
      variableWidth <- Iterator(false, true)
      earlyChange <- Iterator(false, true)
      initialWidth <- {
        val minInitialWidth = Options.minInitialCodeWidth(clearCode, stopCode, variableWidth, earlyChange)
        Iterator.range(0, 3).map(minInitialWidth + _)
      }
      maxWidth <- if (variableWidth) {
        Iterator(None) ++ Iterator.range(1, 4).map(n => Some(initialWidth + n))
      } else {
        Iterator(Some(initialWidth))
      }
      maxDictSize <- (Iterator(None) ++ Iterator.range(BytesAlphabetSize, 2 * BytesAlphabetSize).map(Some(_))).take(1) // !!
      packingOrder <- Iterator(BitSignificance.LSB, BitSignificance.MSB)
      options = Options(
        codeWidth = CodeWidthOptions(
          initialWidth = initialWidth,
          maximumWidth = maxWidth,
          earlyChange = earlyChange
        ),
//        maxDictionarySize = maxDictSize, !!
//        clearCode = clearCode, !!
//        stopCode = stopCode !!
        packingOrder = packingOrder
      )
    } yield options

  private def getContentByResets: Array[Array[Array[Byte]]] = {
    val content = loadContent()
    for (resets <- 0 to MaxResets)
      yield split(content.toVector, resets + 1)
  }.map(_.map(_.toArray).toArray).toArray

  private def loadContent(): Array[Byte] = {
    val zipStream = new ZipInputStream(getClass.getResourceAsStream("/shakespeares-works_TXT_FolgerShakespeare.zip"))
    try {
      val Filename = "hamlet_TXT_FolgerShakespeare.txt"
      while (zipStream.getNextEntry.getName != Filename) {}

      val accumulator = ArrayBuffer.empty[Byte]
      val buffer = new Array[Byte](1024)

      @tailrec
      def read(): Array[Byte] = {
        val count = zipStream.read(buffer)
        if (count == -1) {
          accumulator.toArray
        } else {
          if (count < buffer.length) {
            accumulator.addAll(buffer.take(count))
          } else {
            accumulator.addAll(buffer)
          }
          read()
        }
      }

      read()
    } finally {
      zipStream.close()
    }
  }
}
