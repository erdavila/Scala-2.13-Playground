package lzw

sealed trait CodeConfig

object CodeConfig {
  case class FixedWidth(width: Int) extends CodeConfig
  case class VariableWidth(initialWidth: Int, maximumWidth: Option[Int], earlyChange: Boolean = false) extends CodeConfig
}
