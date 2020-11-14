package lzw.streams

import java.io.ByteArrayInputStream
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

class LzwDecoderInputStreamTest extends AnyFunSuite with InputStreamTestsCommon with DecoderStreamTestsCommon {

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
