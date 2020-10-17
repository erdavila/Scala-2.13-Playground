package lzw.streams

import java.io.ByteArrayInputStream
import lzw.bytes.LzwByteEncoder
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

class LzwDecoderInputStreamTest extends AnyFunSuite with InputStreamTestsCommon {

  private val encodedContent = {
    val encoder = new LzwByteEncoder(options)
    val unfinishedBytes = encoder.encode(decodedBytesInput)
    unfinishedBytes ++ encoder.finish()
  }

  for ((name, getConsumeFunction) <- ReadMethodOverridesCases) {
    testsFor(decoding(name)(getConsumeFunction))
  }

  private def decoding(name: String)(getConsumeFunction: GetConsumeFunction): Unit =
    test(name) {
      val encodedContentInputStream = new ByteArrayInputStream(encodedContent)
      val decoderInputStream = new LzwDecoderInputStream(encodedContentInputStream, options)
      val decodedBytesBuffer = mutable.Buffer.empty[Byte]
      val consume = getConsumeFunction(decoderInputStream, bytes => decodedBytesBuffer.appendAll(bytes))

      while (consume()) {}

      val decodedBytesOutput = decodedBytesBuffer.toArray

      assert(decodedBytesOutput `sameElements` decodedBytesInput, (decodedBytesOutput.length, decodedBytesInput.length))
    }
}
