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
  require(codeWidth.initialWidth >= Options.minInitialCodeWidth(alphabet.size, clearCode, stopCode))
  require(maxDictionarySize.forall(_ >= alphabet.size))
}

object Options {
  def minInitialCodeWidth(alphabetSize: Int, clearCode: Option[Code], stopCode: Option[Code]): Int = {
    val numberOfCodes = alphabetSize + clearCode.size + stopCode.size
    val highestCode = ((numberOfCodes - 1) +: (clearCode ++ stopCode).toSeq).max
    BitUtils.bitsRequired(highestCode)
  }
}
