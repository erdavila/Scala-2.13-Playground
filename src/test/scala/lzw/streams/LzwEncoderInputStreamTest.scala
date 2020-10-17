package lzw.streams

import java.io.ByteArrayInputStream
import lzw.bytes.LzwByteDecoder
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable

class LzwEncoderInputStreamTest extends AnyFunSuite with InputStreamTestsCommon {

  for ((name, getConsumeFunction) <- ReadMethodOverridesCases) {
    testsFor(encoding(name)(getConsumeFunction))
  }

  private def encoding(name: String)(getConsumeFunction: GetConsumeFunction): Unit =
    test(name) {
      val decodedContentInputStream = new ByteArrayInputStream(decodedBytesInput)
      val encoderInputStream = new LzwEncoderInputStream(decodedContentInputStream, options)
      val encodedBytesBuffer = mutable.Buffer.empty[Byte]
      val consume = getConsumeFunction(encoderInputStream, bytes => encodedBytesBuffer.appendAll(bytes))

      while (consume()) {}

      val decodedBytesOutput = new LzwByteDecoder(options).decode(encodedBytesBuffer.toArray)

      assert(decodedBytesOutput `sameElements` decodedBytesInput, (decodedBytesOutput.length, decodedBytesInput.length))
    }
}
