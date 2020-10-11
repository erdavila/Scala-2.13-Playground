package lzw.bytes

import lzw.bits.BitSignificance
import lzw.bytes.Options.BytesAlphabetSize
import lzw.core
import lzw.core.Code

case class Options(
  codeWidth: CodeWidthOptions,
  maxDictionarySize: Option[Int] = None,
  clearCode: Option[Code] = None,
  stopCode: Option[Code] = None,
  packingOrder: BitSignificance = BitSignificance.LSB
) {
  private[bytes] def toCoreOptions: core.Options[Byte] =
    core.Options(
      alphabet = (0 until BytesAlphabetSize).map(_.toByte),
      codeWidth = codeWidth,
      maxDictionarySize = maxDictionarySize,
      clearCode = clearCode,
      stopCode = stopCode,
    )
}

object Options {

  val BytesAlphabetSize = 256

  def minInitialCodeWidth(
    clearCode: Option[Code],
    stopCode: Option[Code],
    variableWidth: Boolean,
    earlyChange: Boolean
  ): Int =
    core.Options.minInitialCodeWidth(BytesAlphabetSize, clearCode, stopCode, variableWidth, earlyChange)
}
