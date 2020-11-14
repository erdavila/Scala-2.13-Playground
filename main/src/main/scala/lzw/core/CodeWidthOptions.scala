package lzw.core

case class CodeWidthOptions(initialWidth: Int, maximumWidth: Option[Int], earlyChange: Boolean = false) {
  require(!maximumWidth.exists(_ < initialWidth))
}

object CodeWidthOptions {
  def fixedWidth(width: Int): CodeWidthOptions =
    new CodeWidthOptions(width, Some(width), earlyChange = false)
}
