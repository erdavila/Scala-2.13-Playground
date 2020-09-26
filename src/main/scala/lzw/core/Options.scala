package lzw.core

import lzw.bits.BitUtils

case class Options[Sym](
  alphabet: Seq[Sym],
  codeWidth: CodeWidthOptions,
  maxDictionarySize: Option[Int] = None,
  clearCode: Option[Code] = None,
  stopCode: Option[Code] = None,
) {
  require(alphabet.sizeIs >= 2)
  require(
    codeWidth.initialWidth >= Options.minInitialCodeWidth(
      alphabet.size,
      clearCode,
      stopCode,
      variableWidth = !codeWidth.maximumWidth.contains(codeWidth.initialWidth),
      earlyChange = codeWidth.earlyChange
    ),
    "initialWidth too small"
  )
  require(maxDictionarySize.forall(_ >= alphabet.size))
}

object Options {
  def minInitialCodeWidth(
    alphabetSize: Int,
    clearCode: Option[Code],
    stopCode: Option[Code],
    variableWidth: Boolean,
    earlyChange: Boolean
  ): Int = {
    val numberOfCodes = alphabetSize + clearCode.size + stopCode.size
    val highestCode = ((numberOfCodes - 1) +: (clearCode ++ stopCode).toSeq).max
    val delta =
      if (!variableWidth) 0
      else if (!earlyChange) 1
      else 2
    BitUtils.bitsRequired(highestCode + delta)
  }
}
