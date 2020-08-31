package lzw

import scala.annotation.tailrec

class BitString private(private val units: Array[BitString.UnitType], len: Int) {
  import BitString._

  require(requiredUnitsForLength(len) == units.length)
  require(
    units.lastOption
      .map { bits =>
        val unusedLength = UnitSize - len % UnitSize
        unusedLength == UnitSize || ((bits >>> unusedLength) == 0)
      }
      .forall(identity)
  )

  @inline def length: Int = len

  def apply(i: Int): Boolean = {
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
      val units = new Array[UnitType](len)
      copyBits(this.units, 0, units, 0, this.length)
      copyBits(lsb.units, 0, units, this.length, len - this.length)
      new BitString(units, len)
    }

  def padMsb(len: Int, elem: Boolean): BitString = {
    require(len >= 0)
    val paddingLength = len - this.length
    val units = Array.fill[UnitType](requiredUnitsForLength(paddingLength))(0)
    val padding = new BitString(units, paddingLength)
    padding ++ this
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
      copyBits(this.units, from, units, 0, len)
      new BitString(units, len)
    }
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

  override def equals(o: Any): Boolean = ???

  override def hashCode(): Int = ???

  override def toString: String =
    (0 until length)
      .map(i => if (this(i)) '1' else '0')
      .mkString
      .reverse

  def toString(groupSize: Int): String =
    Iterator.range(0, length, groupSize)
      .map(n => slice(n, n + groupSize).padMsb(groupSize, elem = false))
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

  def requiredUnitsForLength(bitCount: Int, unitSize: Int = UnitSize): Int =
    (bitCount + (unitSize - 1)) / unitSize
}
