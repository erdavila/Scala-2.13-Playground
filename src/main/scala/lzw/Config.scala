package lzw

case class Config[Sym](
  alphabet: Seq[Sym],
  codeConfig: CodeConfig,
  maxDictionarySize: Option[Int] = None,
  clearCode: Option[Code] = None,
  stopCode: Option[Code] = None,
) {
  require(codeConfig.initialWidth >= BitUtils.bitsRequired(alphabet.size))
  for (c <- clearCode) { require(codeConfig.initialWidth >= BitUtils.bitsRequired(c)) }
  require(!maxDictionarySize.exists(_ < alphabet.size))
}
