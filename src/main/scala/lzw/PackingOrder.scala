package lzw

sealed trait PackingOrder {
  def apply(bitString: BitString): bitString.End
}

object PackingOrder {
  case object LSBFirst extends PackingOrder {
    override def apply(bitString: BitString): bitString.End = bitString.lsb
  }

  case object MSBFirst extends PackingOrder {
    override def apply(bitString: BitString): bitString.End = bitString.msb
  }
}
