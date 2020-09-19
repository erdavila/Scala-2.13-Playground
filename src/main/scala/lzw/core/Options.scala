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
  require(codeWidth.initialWidth >= BitUtils.bitsRequired(alphabet.size + clearCode.size + stopCode.size - 1))
  require(maxDictionarySize.forall(_ >= alphabet.size))
}
