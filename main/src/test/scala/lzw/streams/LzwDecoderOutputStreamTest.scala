package lzw.streams

import java.io.{ByteArrayOutputStream, OutputStream}
import org.scalatest.funsuite.AnyFunSuite

class LzwDecoderOutputStreamTest extends AnyFunSuite with OutputStreamTestsCommon with DecoderStreamTestsCommon {

  for ((name, writeTo) <- writeMethodOverridesCases(encodedContent)) {
    testsFor(decoding(name)(writeTo))
  }

  private def decoding(name: String)(writeTo: OutputStream => Any): Unit =
    test(name) {
      val decodedContentOutputStream = new ByteArrayOutputStream()
      val lzwDecoderOutputStream = new LzwDecoderOutputStream(decodedContentOutputStream, options)

      writeTo(lzwDecoderOutputStream)
      lzwDecoderOutputStream.close()

      val decodedBytesOutput = decodedContentOutputStream.toByteArray

      assert(decodedBytesOutput `sameElements` decodedBytesInput, (decodedBytesOutput.length, decodedBytesInput.length))
    }
}
