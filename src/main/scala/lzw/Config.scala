package lzw

case class Config[Sym](
  alphabet: Seq[Sym],
  codeConfig: CodeConfig,
  clearCode: Boolean = false,
  stopCode: Boolean = false,
)
