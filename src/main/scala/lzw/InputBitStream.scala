package lzw

import java.io.InputStream
import scala.annotation.tailrec

class InputBitStream(inputStream: InputStream, packingOrder: PackingOrder) {

  private var bufferedBits: BitString = BitString.empty

  def get(count: Int): BitString =
    packingOrder match {
      case PackingOrder.LSB =>
        if (count > bufferedBits.length) {
          val bytesRequired = BitString.requiredUnitsForLength(count - bufferedBits.length, java.lang.Byte.SIZE)
          bufferedBits = readBytes(bytesRequired)
            .map(BitString.from)
            .reverse
            .foldRight(bufferedBits)(_ ++ _)
        }

        val (newBufferedBits, result) = bufferedBits.splitLsbAt(count)
        bufferedBits = newBufferedBits
        result
      case PackingOrder.MSB =>
        if (count > bufferedBits.length) {
          val bytesRequired = BitString.requiredUnitsForLength(count - bufferedBits.length, java.lang.Byte.SIZE)
          bufferedBits = readBytes(bytesRequired)
            .map(BitString.from)
            .foldLeft(bufferedBits)(_ ++ _)
        }

        val (result, newBufferedBits) = bufferedBits.splitMsbAt(count)
        bufferedBits = newBufferedBits
        result
    }

  private def readBytes(count: Int): Array[Byte] = {
    val bytes = new Array[Byte](count)

    @tailrec
    def loop(offset: Int): Int = {
      val toRead = bytes.length - offset
      val wereRead = inputStream.read(bytes, offset, toRead)
      if (wereRead == -1) {
        offset
      } else if (wereRead < toRead) {
        loop(offset + wereRead)
      } else {
        offset + wereRead
      }
    }

    val wereRead = loop(0)
    bytes.take(wereRead)
  }
}
