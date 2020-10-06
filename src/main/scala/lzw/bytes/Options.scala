package lzw.bytes

import lzw.bits.BitSignificance
import lzw.bytes.Options.BytesAlphabetSize
import lzw.core
import lzw.core.Code

case class Options(
  codeWidth: CodeWidthOptions,
  packingOrder: BitSignificance = BitSignificance.LSB
) {
  private[bytes] def toCoreOptions: core.Options[Byte] =
    core.Options(
      alphabet = (0 until BytesAlphabetSize).map(_.toByte),
      codeWidth = codeWidth
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
