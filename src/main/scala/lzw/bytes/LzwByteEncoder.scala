package lzw.bytes

import lzw.bits.BitStringDecoder
import lzw.core.LzwEncoder

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
    stats.countEncodedBytes(
      lzwEncoder.encode(bytes.toIndexedSeq)
        .toArray
        .flatMap(bitsDecoder.decode)
    )

  def reset(): Array[Byte] =
    stats.countEncodedBytes(
      lzwEncoder.reset()
        .toArray
        .flatMap(bitsDecoder.decode)
    )

  def finish(): Array[Byte] =
    stats.countEncodedBytes {
      val remainingBytes = lzwEncoder.finish()
        .toArray
        .flatMap(bitsDecoder.decode)

      val lastByteOption = bitsDecoder.finish()

      remainingBytes ++ lastByteOption
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
