package lzw.streams

import java.io.ByteArrayInputStream
import lzw.bytes.{LzwByteDecoder, Options}
import lzw.utils.VaryingOptionsRunnerSupport
import scala.annotation.tailrec
import scala.collection.mutable

object LzwEncoderInputStreamRoundTripTestsRunner {

  private sealed trait Method { def next: Method }
  private object Method {
    case object SingleByte extends Method { override def next: Method = ByteArray }
    case object ByteArray extends Method { override def next: Method = ByteArraySection }
    case object ByteArraySection extends Method { override def next: Method = SingleByte }
  }

  def main(args: Array[String]): Unit =
    VaryingOptionsRunnerSupport.run(processCase)

  def processCase(options: Options, resets: Int): Unit = {
    val contentInputStream = new ByteArrayInputStream(VaryingOptionsRunnerSupport.decodedContent)
    val encoderInputStream = new LzwEncoderInputStream(contentInputStream, options)

    val parts = resets + 1
    val Gap = 2
    val readBuffer = new Array[Byte](2 * Gap + 1 + VaryingOptionsRunnerSupport.decodedContent.length / (3 * (parts + 1)))
    val encodedBytesBuffer = mutable.Buffer.empty[Byte]

    val reusableSingleByteArray = new Array[Byte](1)
    def runMethod(method: Method): Array[Byte] = {
      @inline def ifNotEnded(i: Int)(f: => Array[Byte]): Array[Byte] = if (i != -1) f else Array.emptyByteArray
      method match {
        case Method.SingleByte =>
          val byteValue = encoderInputStream.read()
          ifNotEnded(byteValue) {
            reusableSingleByteArray(0) = byteValue.toByte
            reusableSingleByteArray
          }
        case Method.ByteArray =>
          val readCount = encoderInputStream.read(readBuffer)
          ifNotEnded(readCount) {
            readBuffer.take(readCount)
          }
        case Method.ByteArraySection =>
          val readCount = encoderInputStream.read(readBuffer, Gap, readBuffer.length - 2 * Gap)
          ifNotEnded(readCount) {
            readBuffer.slice(Gap, Gap + readCount)
          }
      }
    }

    @tailrec
    def loop(method: Method = Method.SingleByte, resetsDone: Int = 0): Unit = {
      val consumedBytes = VaryingOptionsRunnerSupport.decodedContent.length - contentInputStream.available()
      val expectedResets = consumedBytes * parts / VaryingOptionsRunnerSupport.decodedContent.length
      if (resetsDone < expectedResets && resetsDone < resets) {
        encoderInputStream.resetEncoding()
        loop(method, resetsDone + 1)
      } else {
        val bytes = runMethod(method)
        if (bytes.nonEmpty) {
          encodedBytesBuffer.appendAll(bytes)
          loop(method.next, resetsDone)
        }
      }
    }

    loop()
    val encodedBytes = encodedBytesBuffer.toArray

    val lzwByteDecoder = new LzwByteDecoder(options)
    val decodedBytes = lzwByteDecoder.decode(encodedBytes)
    val expectedDecodedBytes = VaryingOptionsRunnerSupport.decodedContent
    assert(decodedBytes `sameElements` expectedDecodedBytes, (decodedBytes.length, expectedDecodedBytes.length))
  }
}
