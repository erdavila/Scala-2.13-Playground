package lzw.core

import lzw.bits.BitUtils

case class Options[Sym](
  alphabet: Seq[Sym],
  codeWidth: CodeWidthOptions,
  maxDictionarySize: Option[Int] = None,
  clearCode: Option[Code] = None,
  stopCode: Option[Code] = None,
) {
  require(alphabet.nonEmpty)
  require(codeWidth.initialWidth >= BitUtils.bitsRequired(alphabet.size))
  for (c <- clearCode) { require(codeWidth.initialWidth >= BitUtils.bitsRequired(c)) }
  for (s <- stopCode) { require(codeWidth.initialWidth >= BitUtils.bitsRequired(s)) }
  require(!maxDictionarySize.exists(_ < alphabet.size))
}
