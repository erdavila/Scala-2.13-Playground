package lzw.bits

import java.util
import lzw.bits.BitString.{WordSize, WordType}
import scala.annotation.tailrec

sealed trait BitString { self =>
  def length: Int
  def apply(i: Int): Boolean

  @inline final def ++(lsb: BitString): BitString = concat(lsb)

  def concat(lsb: BitString): BitString

  sealed trait End {
    val significance: BitSignificance

    def otherEnd: End
    def bytesIterator: Iterator[Byte]

    def extend(bits: BitString): BitString

    final def padTo(len: Int, elem: Boolean = false): BitString = {
      val paddingLen = len - length

      val padding =
        if (paddingLen <= 0) {
          EmptyBitString
        } else if (paddingLen <= WordSize) {
          val newWord = if (elem) BitUtils.lsbMask(paddingLen) else 0
          new WordBitString(newWord, paddingLen)
        } else {
          val newWords = new Array[WordType](BitString.requiredPiecesForLength(paddingLen, WordSize))
          if (elem) {
            val lastWordIndex = newWords.length - 1
            for (i <- 0 until lastWordIndex) {
              newWords(i) = ~0
            }
            newWords(lastWordIndex) = BitUtils.lsbMask(paddingLen % WordSize)
          }
          new WordArrayBitString(newWords, paddingLen)
        }

      extend(padding)
    }

    def take(n: Int): BitString
    def drop(n: Int): BitString

    final def splitAt(i: Int): (BitString, BitString) = (take(i), drop(i))
  }

  sealed trait LsbEnd extends End {
    final val significance = BitSignificance.LSB
    override final def otherEnd: End = msb
    override final def extend(bits: BitString): BitString = self ++ bits
  }

  sealed trait MsbEnd extends End {
    final val significance = BitSignificance.MSB
    override final def otherEnd: End = lsb
    override final def extend(bits: BitString): BitString = bits ++ self
    override final def take(n: Int): BitString = lsb.drop(length - n)
    override final def drop(n: Int): BitString = lsb.take(length - n)
  }

  def lsb: LsbEnd
  def msb: MsbEnd

  final def end(significance: BitSignificance): End =
    significance match {
      case BitSignificance.LSB => lsb
      case BitSignificance.MSB => msb
    }

  override final def toString: String =
    iterator
      .map(bitToChar)
      .mkString
      .reverse

  final def toString(groupLen: Int): String =
    iterator.grouped(groupLen)
      .map(
        _.map(bitToChar)
          .padTo(groupLen, false)
          .mkString
      )
      .mkString(" ")
      .reverse

  private def iterator: Iterator[Boolean] =
    Iterator.range(0, length).map(apply)

  private def bitToChar(bit: Boolean): Char =
    if (bit) '1' else '0'
}

object BitString {
  @inline private[bits] final val WordSize = java.lang.Integer.SIZE
  @inline private[bits] final type WordType = Int

  val empty: BitString = EmptyBitString

  def from(byte: Byte): BitString = new WordBitString(byte & 0x000000FF, java.lang.Byte.SIZE)
  def from(short: Short): BitString = new WordBitString(short & 0x0000FFFF, java.lang.Short.SIZE)
  def from(int: Int): BitString = new WordBitString(int, java.lang.Integer.SIZE)
  def from(long: Long): BitString = new WordArrayBitString(Array(long, long >>> WordSize).map(_.toInt), java.lang.Long.SIZE)

  def parse(string: String): BitString = {
    def parseWord(string: String): WordType =
      string.foldLeft(0) { (word, char) =>
        require(char == '0' || char == '1')
        val bit = char - '0'
        (word << 1) | bit
      }

    if (string.isEmpty) {
      EmptyBitString
    } else if (string.sizeIs <= WordSize) {
      val word = parseWord(string)
      new WordBitString(word, string.length)
    } else {
      val words = string.reverse
        .grouped(WordSize)
        .map(str => parseWord(str.reverse))
        .toArray
      new WordArrayBitString(words, string.length)
    }
  }

  private[bits] def requiredPiecesForLength(bitCount: Int, pieceSize: Int): Int =
    (bitCount + (pieceSize - 1)) / pieceSize

  private[bits] def bitAt(word: WordType, i: Int): Boolean =
    ((word >> i) & 0x01) == 1
}

private object EmptyBitString extends BitString {
  override def length: Int = 0
  override def apply(i: Int): Boolean = throw new IndexOutOfBoundsException(i.toString)

  override def concat(lsb: BitString): BitString = lsb

  override lazy val lsb: LsbEnd = new LsbEnd {
    override def bytesIterator: Iterator[Byte] = Iterator.empty
    override final def take(n: Int): BitString = EmptyBitString
    override final def drop(n: Int): BitString = EmptyBitString
  }

  override lazy val msb: MsbEnd = new MsbEnd {
    override def bytesIterator: Iterator[Byte] = Iterator.empty
  }
}

private class WordBitString(
  private[bits] val word: WordType,
  private[bits] val len: Int,
) extends BitString { self =>
//  require(len > 0)
//  require(len <= java.lang.Integer.SIZE)
//  require(len == java.lang.Integer.SIZE || (word >>> len) == 0, (word, len, this.toString))

  @inline override def length: Int = len

  override def apply(i: Int): Boolean =
    BitString.bitAt(word, i)

  override def concat(lsb: BitString): BitString =
    lsb match {
      case EmptyBitString => self
      case that: WordBitString if this.len + that.len <= WordSize =>
        val newWord = (this.word << that.len) | that.word
        new WordBitString(newWord, this.len + that.len)
      case that: WordBitString => WordArrayBitString.concat(Array(this.word), this.len, Array(that.word), that.len)
      case that: WordArrayBitString => WordArrayBitString.concat(Array(this.word), this.len, that.words, that.len)
    }

  override lazy val lsb: LsbEnd = new LsbEnd {
    override final def bytesIterator: Iterator[Byte] =
      Iterator.iterate(word)(_ >>> java.lang.Byte.SIZE)
        .map(_.toByte)
        .take(BitString.requiredPiecesForLength(len, java.lang.Byte.SIZE))

    override def take(n: Int): BitString =
      if (n <= 0) {
        EmptyBitString
      } else if (n < len) {
        val newWord = word & BitUtils.lsbMask(n)
        new WordBitString(newWord, n)
      } else {
        self
      }

    override def drop(n: Int): BitString =
      if (n <= 0) {
        self
      } else if (n < len) {
        val newWord = word >>> n
        new WordBitString(newWord, len - n)
      } else {
        EmptyBitString
      }
  }

  override lazy val msb: MsbEnd = new MsbEnd {
    override def bytesIterator: Iterator[Byte] =
      Iterator.iterate(len)(_ - java.lang.Byte.SIZE)
        .map { bitsRemaining =>
          val offset = bitsRemaining - java.lang.Byte.SIZE
          val value = if (offset >= 0) {
            word >>> offset
          } else {
            word << (java.lang.Byte.SIZE - bitsRemaining)
          }
          value.toByte
        }
        .take(BitString.requiredPiecesForLength(len, java.lang.Byte.SIZE))
  }

  override def equals(o: Any): Boolean =
    o match {
      case that: WordBitString => this.word == that.word && this.len == that.len
      case _ => false
    }

  override def hashCode(): Int =
    word * 31 + len
}

private class WordArrayBitString(
  private[bits] val words: Array[WordType],
  private[bits] val len: Int,
) extends BitString { self =>
//  require(len > java.lang.Integer.SIZE)
//  require(words.length == BitString.requiredPiecesForLength(len, WordSize))
//  require(len % java.lang.Integer.SIZE == 0 || (words.last >>> (len % java.lang.Integer.SIZE)) == 0)

  @inline override def length: Int = len

  override def apply(i: Int): Boolean = {
    val wordIndex = i / WordSize
    val bitIndex = i % WordSize
    BitString.bitAt(words(wordIndex), bitIndex)
  }

  override def concat(lsb: BitString): BitString =
    lsb match {
      case EmptyBitString => self
      case that: WordBitString => WordArrayBitString.concat(this.words, this.len, Array(that.word), that.len)
      case that: WordArrayBitString => WordArrayBitString.concat(this.words, this.len, that.words, that.len)
    }

  override lazy val lsb: LsbEnd = new LsbEnd {
    override def bytesIterator: Iterator[Byte] =
      Iterator.iterate(0)(_ + java.lang.Byte.SIZE)
        .map { bitIndex =>
          val wordIndex = bitIndex / WordSize
          val bitOffset = bitIndex % WordSize
          (words(wordIndex) >>> bitOffset).toByte
        }
        .take(BitString.requiredPiecesForLength(len, java.lang.Byte.SIZE))

    override def take(n: Int): BitString = slice(0, n)
    override def drop(n: Int): BitString = slice(n, len)
  }

  override lazy val msb: MsbEnd = new MsbEnd {
    override def bytesIterator: Iterator[Byte] =
      Iterator.iterate(len)(_ - java.lang.Byte.SIZE)
        .map { bitsRemaining =>
          val offset = bitsRemaining - java.lang.Byte.SIZE
          val value = if (offset >= 0) {
            val wordIndex = offset / WordSize
            val bitOffset = offset % WordSize

            val lowBitsCount = bitOffset
            val highBitsCount = WordSize - bitOffset

            val low = words(wordIndex) >>> lowBitsCount
            val high = if (highBitsCount < java.lang.Byte.SIZE) {
              words(wordIndex + 1) << highBitsCount
            } else {
              0
            }

            low | high
          } else {
            words(0) << (java.lang.Byte.SIZE - bitsRemaining)
          }
          value.toByte
        }
        .take(BitString.requiredPiecesForLength(len, java.lang.Byte.SIZE))
  }

  private def slice(from: Int, until: Int): BitString = {
    def inRange(i: Int) =
      if (i < 0) 0
      else if (i > len) len
      else i

    val effectiveFrom = inRange(from)
    val effectiveUntil = inRange(until)
    val effectiveLen = effectiveUntil - effectiveFrom

    if (effectiveLen == 0) {
      EmptyBitString
    } else if (effectiveLen <= WordSize) {
      val newWords = Array[WordType](0)
      WordArrayBitString.copyBits(words, effectiveFrom, newWords, 0, effectiveLen)
      new WordBitString(newWords(0), effectiveLen)
    } else if (effectiveLen == len) {
      self
    } else {
      val newWords = new Array[WordType](BitString.requiredPiecesForLength(effectiveLen, WordSize))
      WordArrayBitString.copyBits(words, effectiveFrom, newWords, 0, effectiveLen)
      new WordArrayBitString(newWords, effectiveLen)
    }
  }

  override def equals(o: Any): Boolean =
    o match {
      case that: WordArrayBitString =>
        (this.words `sameElements` that.words) && this.len == that.len
      case _ => false
    }

  override def hashCode(): Int =
    util.Arrays.hashCode(words) * 31 + len
}

private object WordArrayBitString {
  private[bits] def concat(msbWords: Array[WordType], msbLen: Int, lsbWords: Array[WordType], lsbLen: Int): WordArrayBitString = {
    val newLen = msbLen + lsbLen
    val newWords = new Array[WordType](BitString.requiredPiecesForLength(newLen, WordSize))
    copyBits(lsbWords, 0, newWords, 0, lsbLen)
    copyBits(msbWords, 0, newWords, lsbLen, msbLen)
    new WordArrayBitString(newWords, newLen)
  }

  private def copyBits(fromWords: Array[WordType], fromBitIndex: Int, toWords: Array[WordType], toBitIndex: Int, count: Int): Unit = {
    @tailrec
    def loop(fromBitIndex: Int, toBitIndex: Int, count: Int): Unit =
      if (count > 0) {
        val fromWordIndex = fromBitIndex / WordSize
        val fromBitOffset = fromBitIndex % WordSize

        val toWordIndex = toBitIndex / WordSize
        val toBitOffset = toBitIndex % WordSize

        val copyLen = Seq(count, WordSize - fromBitOffset, WordSize - toBitOffset).min

        val bits = (fromWords(fromWordIndex) >>> fromBitOffset) & BitUtils.lsbMask(copyLen)
        toWords(toWordIndex) |= bits << toBitOffset

        loop(fromBitIndex + copyLen, toBitIndex + copyLen, count - copyLen)
      }

    loop(fromBitIndex, toBitIndex, count)
  }
}
