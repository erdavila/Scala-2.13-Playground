package lzw.core

import lzw.bits.BitString

class CodeProducer(codeWidthOptions: CodeWidthOptions, firstNextCode: Code) {
  private var width: Int = codeWidthOptions.initialWidth
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
        !codeWidthOptions.maximumWidth.forall(width < _)

      val delta = if (codeWidthOptions.earlyChange) 1 else 0
      if (code + delta == widthIncreaseCode && !isMaxWidthExhausted) {
        width += 1
        widthIncreaseCode <<= 1
      }

      code
    }

  def toBitString(code: Code): BitString =
    BitString.from(code).lsb.take(width)
}
