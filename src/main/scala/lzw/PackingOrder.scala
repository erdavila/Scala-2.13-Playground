package lzw

sealed trait PackingOrder

object PackingOrder {
  case object LSB extends PackingOrder
  case object MSB extends PackingOrder
}
