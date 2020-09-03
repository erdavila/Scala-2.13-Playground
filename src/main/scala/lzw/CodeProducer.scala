package lzw

class CodeProducer(codeConfig: CodeConfig, firstNextCode: Code) {
  private var width: Int = codeConfig.initialWidth
  private var widthIncreaseCode: Code = 1 << width
  private var isMaxWidthExhausted: Boolean = false

  private var theNextCode: Code = firstNextCode

  def maxWidthExhausted: Boolean = isMaxWidthExhausted

  def nextCode: Option[Code] =
    Option.when(!isMaxWidthExhausted) {
      val code = theNextCode
      theNextCode += 1
      isMaxWidthExhausted =
        theNextCode == widthIncreaseCode &&
        !codeConfig.maximumWidth.forall(width < _)

      val delta = if (codeConfig.earlyChange) 1 else 0
      if (code + delta == widthIncreaseCode && !isMaxWidthExhausted) {
        width += 1
        widthIncreaseCode <<= 1
      }

      code
    }

  def toBitString(code: Code): BitString =
    BitString.from(code).lsb.take(width)
}
