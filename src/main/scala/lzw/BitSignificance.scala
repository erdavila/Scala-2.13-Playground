package lzw

sealed trait BitSignificance

object BitSignificance {
  case object LSB extends BitSignificance
  case object MSB extends BitSignificance
}
