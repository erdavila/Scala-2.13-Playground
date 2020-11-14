package lzw.bytes

import lzw.bits.BitStringDecoder
import lzw.core.LzwEncoder
import scala.collection.mutable

class LzwByteEncoder(options: Options) {

  private val lzwEncoder = new LzwEncoder(options.toCoreOptions)
  private val bitsDecoder = new BitStringDecoder(options.packingOrder)

  private object stats {
    var encodedBytesCount: Int = 0

    def countEncodedBytes(bytes: Array[Byte]): Array[Byte] = {
      encodedBytesCount += bytes.length
      bytes
    }
  }

  def encode(bytes: Array[Byte]): Array[Byte] =
    stats.countEncodedBytes {
      val buffer = mutable.Buffer.empty[Byte]
      for (bitString <- lzwEncoder.encode(bytes.toIndexedSeq)) {
        buffer.appendAll(bitsDecoder.decode(bitString))
      }
      buffer.toArray
    }

  def reset(): Array[Byte] =
    stats.countEncodedBytes {
      val buffer = mutable.Buffer.empty[Byte]
      for (bitString <- lzwEncoder.reset()) {
        buffer.appendAll(bitsDecoder.decode(bitString))
      }
      buffer.toArray
    }

  def finish(): Array[Byte] =
    stats.countEncodedBytes {
      val buffer = mutable.Buffer.empty[Byte]
      for (bitString <- lzwEncoder.finish()) {
        buffer.appendAll(bitsDecoder.decode(bitString))
      }
      buffer.appendAll(bitsDecoder.finish())

      buffer.toArray
    }

  def statistics: Statistics = {
    val s = lzwEncoder.statistics
    Statistics(
      decodedBytes = s.inputSymbols,
      encodedBytes = stats.encodedBytesCount,
      dictionarySize = s.dictionarySize,
    )
  }

  def maxCodeWidthExhausted: Boolean = lzwEncoder.maxCodeWidthExhausted
}
