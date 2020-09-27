package lzw.core

import lzw.bits.BitString

class CodeProducer(codeWidthOptions: CodeWidthOptions, reservedCodes: Seq[Code]) {
  private var width: Int = codeWidthOptions.initialWidth
  private var widthIncreaseCode: Code = 1 << width
  private var isMaxWidthExhausted: Boolean = false

  private var _nextCode: Code = 0
  skipReservedCodes()

  def maxWidthExhausted: Boolean = isMaxWidthExhausted

  def nextCode: Option[Code] =
    Option.when(!isMaxWidthExhausted) {
      val code = _nextCode
      _nextCode += 1
      skipReservedCodes()
      isMaxWidthExhausted =
        _nextCode >= widthIncreaseCode &&
        !codeWidthOptions.maximumWidth.forall(width < _)

      val delta = if (codeWidthOptions.earlyChange) 1 else 0
      if (code + delta == widthIncreaseCode && !isMaxWidthExhausted) {
        width += 1
        widthIncreaseCode <<= 1
      }

      code
    }

  private def skipReservedCodes(): Unit =
    while (reservedCodes.contains(_nextCode)) {
      _nextCode += 1
    }

  def toBitString(code: Code): BitString =
    BitString.from(code).lsb.take(width)
}
