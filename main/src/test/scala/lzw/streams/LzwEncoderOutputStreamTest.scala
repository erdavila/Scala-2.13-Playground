package lzw.streams

import java.io.{ByteArrayOutputStream, OutputStream}
import lzw.bytes.LzwByteDecoder
import org.scalatest.funsuite.AnyFunSuite

class LzwEncoderOutputStreamTest extends AnyFunSuite with OutputStreamTestsCommon {

  for ((name, writeTo) <- writeMethodOverridesCases(decodedBytesInput)) {
    testsFor(encoding(name)(writeTo))
  }

  private def encoding(name: String)(writeTo: OutputStream => Any): Unit =
    test(name) {
      val encodedContentOutputStream = new ByteArrayOutputStream()
      val lzwEncoderOutputStream = new LzwEncoderOutputStream(encodedContentOutputStream, options)

      writeTo(lzwEncoderOutputStream)
      lzwEncoderOutputStream.close()

      val decodedBytesOutput = new LzwByteDecoder(options).decode(encodedContentOutputStream.toByteArray)

      assert(decodedBytesOutput `sameElements` decodedBytesInput, (decodedBytesOutput.length, decodedBytesInput.length))
    }
}
