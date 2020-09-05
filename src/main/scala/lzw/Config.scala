package lzw

case class Config(
  symbolWidth: Int,
  alphabetSize: Int,
  code: CodeConfig,
  clearCode: Option[Int] = None,
  stopCode: Option[Int] = None,
  packingOrderFirst: BitSignificance = BitSignificance.LSB,
) {
  def withAutomaticClearCode: Config = ???
  def withAutomaticStopCode: Config = ???
}
