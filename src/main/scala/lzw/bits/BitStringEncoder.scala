package lzw.bits

import scala.collection.mutable

class BitStringEncoder(packingOrderFirst: BitSignificance) {
  private val bytesQueue = mutable.Queue.empty[Byte]
  private var stagingBits: BitString = BitString.empty
  private var totalBits: Int = 0

  def putBytes(bytes: Array[Byte]): Unit = {
    bytesQueue.enqueueAll(bytes)
    totalBits += bytes.length * java.lang.Byte.SIZE
  }

  def bitsAvailable: Int =
    totalBits

  def getBits(n: Int): BitString = {
    while (stagingBits.length < n && bytesQueue.nonEmpty) {
      val byte = bytesQueue.dequeue()
      stagingBits = stagingBits.end(packingOrderFirst).otherEnd.extend(BitString.from(byte))
    }

    val (newStagingBits, bits) = stagingBits.end(packingOrderFirst).splitAt(n)

    stagingBits = newStagingBits
    totalBits -= bits.length

    bits
  }
}
