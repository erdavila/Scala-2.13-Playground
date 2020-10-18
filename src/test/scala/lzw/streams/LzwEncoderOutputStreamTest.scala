package lzw.streams

import java.io.{ByteArrayOutputStream, OutputStream}
import lzw.bytes.LzwByteDecoder
import org.scalatest.funsuite.AnyFunSuite

class LzwEncoderOutputStreamTest extends AnyFunSuite with StreamTestsCommon {

  testsFor(
    encoding("write(Byte)") { out =>
      for (byte <- decodedBytesInput) {
        out.write(byte)
      }
    }
  )

  testsFor(
    encoding("write(Array[Byte])") { out =>
      for (bytes <- decodedBytesInput.grouped(5)) {
        out.write(bytes)
      }
    }
  )

  testsFor(
    encoding("write(Array[Byte], Int, Int)") { out =>
      for (off <- Iterator.range(0, decodedBytesInput.length, step = 5)) {
        val len = math.min(5, decodedBytesInput.length - off)
        out.write(decodedBytesInput, off, len)
      }
    }
  )

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
