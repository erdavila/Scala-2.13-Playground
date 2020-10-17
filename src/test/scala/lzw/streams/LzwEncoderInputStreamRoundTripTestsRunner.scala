package lzw.streams

import java.io.ByteArrayInputStream
import lzw.bytes.{LzwByteDecoder, Options}
import lzw.utils.VaryingOptionsRunnerSupport
import scala.annotation.tailrec
import scala.collection.mutable

object LzwEncoderInputStreamRoundTripTestsRunner {

  def main(args: Array[String]): Unit =
    VaryingOptionsRunnerSupport.run(processCase)

  def processCase(options: Options, resets: Int): Unit = {
    val contentInputStream = new ByteArrayInputStream(VaryingOptionsRunnerSupport.decodedContent)
    val encoderInputStream = new LzwEncoderInputStream(contentInputStream, options)

    val parts = resets + 1
    val Gap = 2
    val readBufferSize = 2 * Gap + 1 + VaryingOptionsRunnerSupport.decodedContent.length / (3 * (parts + 1))
    val encodedBytesBuffer = mutable.Buffer.empty[Byte]
    val readMethodOverrides = new ReadMethodOverrides(encoderInputStream, readBufferSize, bytes => encodedBytesBuffer.appendAll(bytes))

    @tailrec
    def loop(readMethodOverride: readMethodOverrides.Override = readMethodOverrides.singleByte, resetsDone: Int = 0): Unit = {
      val consumedBytes = VaryingOptionsRunnerSupport.decodedContent.length - contentInputStream.available()
      val expectedResets = consumedBytes * parts / VaryingOptionsRunnerSupport.decodedContent.length
      if (resetsDone < expectedResets && resetsDone < resets) {
        encoderInputStream.resetEncoding()
        loop(readMethodOverride, resetsDone + 1)
      } else {
        if (readMethodOverride.execute()) {
          loop(readMethodOverride.next, resetsDone)
        } else {
          assert(resetsDone == resets)
        }
      }
    }

    loop()
    val encodedBytes = encodedBytesBuffer.toArray

    val decodedBytes = new LzwByteDecoder(options).decode(encodedBytes)
    val expectedDecodedBytes = VaryingOptionsRunnerSupport.decodedContent
    assert(decodedBytes `sameElements` expectedDecodedBytes, (decodedBytes.length, expectedDecodedBytes.length))
  }
}
