package lzw.bytes

import lzw.bits.BitStringEncoder
import lzw.core.LzwDecoder
import scala.collection.mutable.ArrayBuffer

class LzwByteDecoder(options: Options) {

  private val bitsEncoder = new BitStringEncoder(options.packingOrder)
  private val lzwDecoder = new LzwDecoder(options.toCoreOptions)

  private object stats {
    var encodedBytesCount: Int = 0

    def countEncodedBytes(bytes: Array[Byte]): Unit =
      encodedBytesCount += bytes.length
  }

  def decode(bytes: Array[Byte]): Array[Byte] = {
    stats.countEncodedBytes(bytes)

    bitsEncoder.putBytes(bytes)

    val arrayBuffer = ArrayBuffer.empty[Byte]
    while (bitsEncoder.bitsAvailable >= lzwDecoder.expectedCodeWidth) {
      val code = bitsEncoder.getBits(lzwDecoder.expectedCodeWidth)
      val output = lzwDecoder.decode(code)
      arrayBuffer.addAll(output)
    }

    arrayBuffer.toArray
  }

  def statistics: Statistics = {
    val s = lzwDecoder.statistics
    Statistics(
      decodedBytes = s.outputSymbols,
      encodedBytes = stats.encodedBytesCount,
      dictionarySize = s.dictionarySize,
    )
  }

  def stopped: Boolean = lzwDecoder.stopped
}
