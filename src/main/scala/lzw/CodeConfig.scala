package lzw

case class CodeConfig(initialWidth: Int, maximumWidth: Option[Int], earlyChange: Boolean) {
  require(!maximumWidth.exists(_ < initialWidth))
}

object CodeConfig {
  def fixedWidth(width: Int): CodeConfig =
    new CodeConfig(width, Some(width), earlyChange = false)
}
