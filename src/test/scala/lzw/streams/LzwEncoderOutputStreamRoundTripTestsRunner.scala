package lzw.streams

import java.io.ByteArrayOutputStream
import lzw.bytes.{LzwByteDecoder, Options}
import lzw.utils.VaryingOptionsRunnerSupport
import scala.annotation.tailrec

object LzwEncoderOutputStreamRoundTripTestsRunner {

  def main(args: Array[String]): Unit =
    VaryingOptionsRunnerSupport.run(processCase)

  def processCase(options: Options, resets: Int): Unit = {
    val contentOutputStream = new ByteArrayOutputStream()
    val encoderOutputStream = new LzwEncoderOutputStream(contentOutputStream, options)

    val parts = resets + 1
    val writeBufferSize = 1 + VaryingOptionsRunnerSupport.decodedContent.length / (3 * (parts + 1))
    val writeMethodOverrides = new WriteMethodOverrides(encoderOutputStream, writeBufferSize, VaryingOptionsRunnerSupport.decodedContent)

    @tailrec
    def loop(writeMethodOverride: writeMethodOverrides.Override = writeMethodOverrides.singleByte, resetsDone: Int = 0): Unit = {
      val expectedResets = writeMethodOverrides.position * parts / VaryingOptionsRunnerSupport.decodedContent.length
      if (resetsDone < expectedResets && resetsDone < resets) {
        encoderOutputStream.resetEncoding()
        loop(writeMethodOverride, resetsDone + 1)
      } else {
        if (writeMethodOverride.execute()) {
          loop(writeMethodOverride.next, resetsDone)
        } else {
          assert(resetsDone == resets)
          encoderOutputStream.close()
        }
      }
    }

    loop()
    val encodedBytes = contentOutputStream.toByteArray

    val decodedBytes = new LzwByteDecoder(options).decode(encodedBytes)
    val expectedDecodedBytes = VaryingOptionsRunnerSupport.decodedContent
    assert(decodedBytes `sameElements` expectedDecodedBytes, (decodedBytes.length, expectedDecodedBytes.length))
  }
}
