package lzw.bytes

import lzw.utils.VaryingOptionsRunnerSupport

object RoundTripTestsRunner {

  private val bestCases = {
    implicit val ordering: Ordering[(Int, Options, Int)] = Ordering.by(_._1)
    new MultiThreadBestValues(50)(ordering)
  }

  def main(args: Array[String]): Unit =
    try {
      VaryingOptionsRunnerSupport.run(processCase)
    } finally {
      println()
      println("Best compression cases (reduction; options; resets):")
      val originalSize = VaryingOptionsRunnerSupport.decodedContent.length
      for ((encodedSize, options, resets) <- bestCases.get()) {
        val sizeReduction = originalSize - encodedSize
        val reductionPercent = 100.0 * sizeReduction / originalSize
        println(s"  $reductionPercent%; $options; $resets")
      }
    }

  private def processCase(options: Options, resets: Int): Unit = {
    val decodedBytesParts = VaryingOptionsRunnerSupport.decodedContentByResets(resets)

    val encoder = new LzwByteEncoder(options)
    val unfinishedEncodedBytes = decodedBytesParts.zipWithIndex
      .map { case (bytes, i) =>
        val flushedEncodedBytes = if (i > 0) encoder.reset() else Array.empty
        flushedEncodedBytes ++ encoder.encode(bytes)
      }
      .reduce(_ ++ _)
    val encodedBytes = unfinishedEncodedBytes ++ encoder.finish()

    val decoder = new LzwByteDecoder(options)
    val decodedBytes = decoder.decode(encodedBytes)
    val expectedDecodedBytes = VaryingOptionsRunnerSupport.decodedContent
    assert(decodedBytes `sameElements` expectedDecodedBytes)

    bestCases.consider((encodedBytes.length, options, resets))
  }
}
