package lzw

class BitString {

  def length: Int = ???

  def apply(i: Int): Boolean = ???

  def ++(lsb: BitString): BitString = ???

  override def equals(o: Any): Boolean = ???

  override def hashCode(): Int = ???

  override def toString: String = ???

  def toString(groupSize: Int): String = ???
}

object BitString {
  val empty: BitString = ???

  def from(byte: Byte): BitString = ???
  def from(short: Short): BitString = ???
  def from(int: Int): BitString = ???
  def from(long: Long): BitString = ???
}
