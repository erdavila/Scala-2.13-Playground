package lzw.bytes

import lzw.bits.BitStringEncoder
import lzw.core.LzwDecoder
import scala.collection.mutable.ArrayBuffer

class LzwByteDecoder(options: Options) {

  private val bitsEncoder = new BitStringEncoder(options.packingOrder)
  private val lzwDecoder = new LzwDecoder(options.toCoreOptions)

  def decode(bytes: Array[Byte]): Array[Byte] = {
    bitsEncoder.putBytes(bytes)

    val arrayBuffer = ArrayBuffer.empty[Byte]
    while (bitsEncoder.bitsAvailable >= lzwDecoder.expectedCodeWidth) {
      val code = bitsEncoder.getBits(lzwDecoder.expectedCodeWidth)
      val output = lzwDecoder.decode(code)
      arrayBuffer.addAll(output)
    }

    arrayBuffer.toArray
  }
}
