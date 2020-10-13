package lzw.streams

import java.io.ByteArrayInputStream
import lzw.bytes.{CodeWidthOptions, LzwByteDecoder, Options}
import org.scalatest.funsuite.AnyFunSuite

class LzwEncoderInputStreamTest extends AnyFunSuite {

  private val options = Options(
    codeWidth = CodeWidthOptions(
      initialWidth = 9,
      maximumWidth = None,
    ),
  )

  private val decodedBytesInput = {
    val X = 'X'.toByte
    val o = 'o'.toByte
    Iterator.iterate(X) {
      case `X` => o
      case `o` => X
    }.take(19).toArray
  }

  testsFor(
    encoding("read()") { encoder =>
      val reusableSingleByteArray = new Array[Byte](1)

      () => {
        val byteValue = encoder.read()
        if (byteValue != -1) {
          reusableSingleByteArray(0) = byteValue.toByte
          reusableSingleByteArray
        } else {
          Array.emptyByteArray
        }
      }
    }
  )

  testsFor(
    encoding("read(Array[Byte])") { encoder =>
      val readBuffer = new Array[Byte](decodedBytesInput.length / 2)

      () => {
        val readCount = encoder.read(readBuffer)
        if (readCount != -1) {
          readBuffer.take(readCount)
        } else {
          Array.emptyByteArray
        }
      }
    }
  )

  testsFor(
    encoding("read(Array[Byte], Int, Int)") { encoder =>
      val GapSize = 2
      val readBuffer = new Array[Byte](decodedBytesInput.length / 2 + 2 * GapSize)

      () => {
        val readCount = encoder.read(readBuffer, GapSize, readBuffer.length - 2 * GapSize)
        if (readCount != -1) {
          readBuffer.slice(GapSize, GapSize + readCount)
        } else {
          Array.emptyByteArray
        }
      }
    }
  )

  private def encoding(name: String)(getConsumeFunction: LzwEncoderInputStream => () => Array[Byte]): Unit =
    test(name) {
      val decodedContentInputStream = new ByteArrayInputStream(decodedBytesInput)
      val encoderInputStream = new LzwEncoderInputStream(decodedContentInputStream, options)

      val consume = getConsumeFunction(encoderInputStream)
      val encodedBytes = Iterator.continually(consume())
        .takeWhile(_.nonEmpty)
        .flatten
        .toArray
      val decodedBytesOutput = new LzwByteDecoder(options).decode(encodedBytes)

      assert(decodedBytesOutput `sameElements` decodedBytesInput, (decodedBytesOutput.length, decodedBytesInput.length))
    }
}
