package lzw

case class Config[Sym](
  alphabet: Seq[Sym],
  codeConfig: CodeConfig,
  maxDictionarySize: Option[Int] = None,
  clearCode: Boolean = false,
  stopCode: Boolean = false,
) {
  require(codeConfig.initialWidth >= BitUtils.bitsRequired(alphabet.size))
  require(!maxDictionarySize.exists(_ < alphabet.size))
}
