package lzw

sealed trait PackingOrder {
  def apply(bitString: BitString): bitString.End
}

object PackingOrder {
  case object LSB extends PackingOrder {
    override def apply(bitString: BitString): bitString.End = bitString.lsb
  }

  case object MSB extends PackingOrder {
    override def apply(bitString: BitString): bitString.End = bitString.msb
  }
}
