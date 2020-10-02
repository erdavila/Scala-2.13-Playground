package lzw.bytes

import lzw.bits.BitSignificance
import lzw.core

case class Options(
  codeWidth: CodeWidthOptions,
  packingOrder: BitSignificance = BitSignificance.LSB
) {
  private[bytes] def toCoreOptions: core.Options[Byte] =
    core.Options(
      alphabet = (0 until 256).map(_.toByte),
      codeWidth = codeWidth
    )
}
