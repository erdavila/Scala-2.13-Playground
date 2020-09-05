package lzw

import scala.annotation.tailrec

class BitString private(private val units: Array[BitString.UnitType], len: Int) { self =>
  import BitString._

  require(requiredUnitsForLength(len) == units.length)
  require(
    units.lastOption
      .map { bits =>
        val usedLength = len % UnitSize
        usedLength == 0 || (bits >>> usedLength) == 0
      }
      .forall(identity)
  )

  @inline def length: Int = len

  def apply(i: Int): Boolean = {
    require(i >= 0)
    require(i < length)
    val unitIndex = i / UnitSize
    val bitIndex = i % UnitSize
    val bit = (units(unitIndex) >> bitIndex) & 0x1
    bit != 0
  }

  def ++(lsb: BitString): BitString =
    if (this.length == 0) lsb
    else if (lsb.length == 0) this
    else {
      val len = this.length + lsb.length
      val units = new Array[UnitType](requiredUnitsForLength(len))
      copyBits(lsb.units, 0, units, 0, lsb.length)
      copyBits(this.units, 0, units, lsb.length, this.length)
      new BitString(units, len)
    }

  def slice(from: Int, until: Int): BitString = {
    require(from >= 0)
    require(until >= from)
    val effectiveFrom = math.min(from, length)
    val effectiveUntil = math.min(until, length)
    val len = effectiveUntil - effectiveFrom
    if (len == 0) {
      BitString.empty
    } else {
      val units = new Array[UnitType](requiredUnitsForLength(len))
      copyBits(this.units, effectiveFrom, units, 0, len)
      new BitString(units, len)
    }
  }

  val lsb: End = new End {
    override val significance: BitSignificance = BitSignificance.LSB

    override def bytes: Iterator[Byte] = {
      val byteCount = requiredUnitsForLength(length, java.lang.Byte.SIZE)
      val bytesPerUnit = UnitSize / java.lang.Byte.SIZE
      units.iterator
        .flatMap { unit =>
          Iterator.iterate(unit)(_ >>> java.lang.Byte.SIZE)
            .take(bytesPerUnit)
        }
        .take(byteCount)
        .map(_.toByte)
    }

    override def drop(n: UnitType): BitString =
      slice(n, length)

    override def extend(bits: BitString): BitString =
      self ++ bits

    override def take(n: Int): BitString =
      slice(0, n)

    override def otherEnd: End = msb
  }

  val msb: End = new End {
    override val significance: BitSignificance = BitSignificance.MSB

    override def bytes: Iterator[Byte] = {
      val byteCount = requiredUnitsForLength(length, java.lang.Byte.SIZE)
      val bytesPerUnit = UnitSize / java.lang.Byte.SIZE
      val extendedBS = self.lsb.padTo(self.units.length * UnitSize)
      extendedBS.units.reverseIterator
        .flatMap { unit =>
          Iterator.iterate(unit)(_ >>> java.lang.Byte.SIZE)
            .take(bytesPerUnit)
            .toSeq
            .reverse
        }
        .take(byteCount)
        .map(_.toByte)
    }

    override def drop(n: UnitType): BitString =
      slice(0, length - n)

    override def extend(bits: BitString): BitString =
      bits ++ self

    override def take(n: Int): BitString =
      slice(length - n, length)

    override def otherEnd: End = lsb
  }

  val end: Map[BitSignificance, End] = Map(BitSignificance.LSB -> lsb, BitSignificance.MSB -> msb)

  sealed trait End {
    val significance: BitSignificance
    def bytes: Iterator[Byte]
    def drop(n: Int): BitString
    def extend(bits: BitString): BitString

    final def padTo(len: UnitType, elem: Boolean = false): BitString = {
      require(len >= 0)
      val paddingLength = len - self.length
      val unit: UnitType = if (elem) 1 else 0
      val units = Array.fill[UnitType](requiredUnitsForLength(paddingLength))(unit)
      val bits = new BitString(units, paddingLength)
      extend(bits)
    }

    final def splitAt(n: Int): (BitString, BitString) = {
      require(n >= 0)
      if (n == 0) (self, BitString.empty)
      else if (n >= length) (BitString.empty, self)
      else (drop(n), take(n))
    }

    def take(n: Int): BitString

    def otherEnd: End
  }

  private def copyBits(fromUnits: Array[UnitType], fromBitIndex: Int, toUnits: Array[UnitType], toBitIndex: Int, count: Int): Unit = {
    def mask(len: Int): UnitType = ~((~1) << (len - 1))

    @tailrec
    def loop(fromBitIndex: Int, toBitIndex: Int, count: Int): Unit =
      if (count > 0) {
        val fromUnitIndex = fromBitIndex / UnitSize
        val fromBitOffset = fromBitIndex % UnitSize

        val toUnitIndex = toBitIndex / UnitSize
        val toBitOffset = toBitIndex % UnitSize

        val copyLen = Seq(count, UnitSize - fromBitOffset, UnitSize - toBitOffset).min

        val bits = (fromUnits(fromUnitIndex) >>> fromBitOffset) & mask(copyLen)
        toUnits(toUnitIndex) |= bits << toBitOffset

        loop(fromBitIndex + copyLen, toBitIndex + copyLen, count - copyLen)
      }

    loop(fromBitIndex, toBitIndex, count)
  }

  override def equals(other: Any): Boolean =
    other match {
      case that: BitString => this.length == that.length && this.units.sameElements(that.units)
      case _ => false
    }

  override def hashCode(): Int =
    len.hashCode() * 31 + units.hashCode()

  override def toString: String =
    (0 until length)
      .map(i => if (this(i)) '1' else '0')
      .mkString
      .reverse

  def toString(groupSize: Int): String =
    Iterator.range(0, length, groupSize)
      .map { n =>
        slice(n, n + groupSize)
          .msb.padTo(groupSize)
      }
      .toSeq
      .reverse
      .mkString(" ")
}

object BitString {
  private type UnitType = Int
  @inline private final val UnitSize = java.lang.Integer.SIZE

  val empty: BitString = new BitString(Array.empty, 0)

  def from(byte: Byte): BitString = new BitString(Array(byte & 0x000000FF), java.lang.Byte.SIZE)
  def from(short: Short): BitString = new BitString(Array(short & 0x0000FFFF), java.lang.Short.SIZE)
  def from(int: Int): BitString = new BitString(Array(int), java.lang.Integer.SIZE)
  def from(long: Long): BitString = new BitString(Array(long, long >>> java.lang.Integer.SIZE).map(_.toInt), java.lang.Long.SIZE)

  def parse(str: String): BitString = {
    val units = str.reverse
      .grouped(UnitSize)
      .map { str =>
        str.reverse.foldLeft(0) { (int, char) =>
          require(char == '0' || char == '1')
          val bit = char - '0'
          (int << 1) | bit
        }
      }
      .toArray
    new BitString(units, str.length)
  }

  def requiredUnitsForLength(bitCount: Int, unitSize: Int = UnitSize): Int =
    (bitCount + (unitSize - 1)) / unitSize
}
