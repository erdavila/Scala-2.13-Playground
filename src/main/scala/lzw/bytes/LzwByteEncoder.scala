package lzw.bytes

import lzw.bits.BitStringDecoder
import lzw.core.LzwEncoder

class LzwByteEncoder(options: Options) {

  private val lzwEncoder = new LzwEncoder(options.toCoreOptions)
  private val bitsDecoder = new BitStringDecoder(options.packingOrder)

  def encode(bytes: Array[Byte]): Array[Byte] =
    lzwEncoder.encode(bytes.toIndexedSeq)
      .toArray
      .flatMap(bitsDecoder.decode)

  def finish(): Array[Byte] = {
    val remainingBytes = lzwEncoder.finish()
      .toArray
      .flatMap(bitsDecoder.decode)

    val lastByteOption = bitsDecoder.finish()

    remainingBytes ++ lastByteOption
  }
}
