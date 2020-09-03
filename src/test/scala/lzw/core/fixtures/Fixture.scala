package lzw.core.fixtures

import lzw.core.Options

case class Fixture[Sym](
  name: String,
  options: Options[Sym],
  symbols: Seq[Sym],
  codesBits: Seq[String],
  dictionarySizeAtTheEnd: Int,
)
