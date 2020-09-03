package lzw

class CodeProducer(codeConfig: CodeConfig, firstNextCode: Code) {
  private var width = codeConfig.initialWidth
  private var widthIncreaseCode: Code = 1 << width

  private var theNextCode: Code = firstNextCode

  def nextCode: Code = {
    val code = theNextCode

    val earlyDelta = if (codeConfig.earlyChange) 1 else 0
    if (code + earlyDelta == widthIncreaseCode) {
      width += 1
      widthIncreaseCode <<= 1
    }

    theNextCode += 1
    code
  }

  def toBitString(code: Code): BitString =
    BitString.from(code).lsb.take(width)
}
