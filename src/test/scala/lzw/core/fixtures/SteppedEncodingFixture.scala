package lzw.core.fixtures

import lzw.core.fixtures.SteppedEncodingFixture.{Encode, Step}
import lzw.core.{LzwEncoder, Options}

case class SteppedEncodingFixture[Sym](
  name: String,
  options: Options[Sym],
  steps: Seq[Step[Sym]],
  dictionarySizeAtTheEnd: Int,
) {
  def toFixture: Fixture[Sym] = Fixture(
    name, options,
    symbols = steps.flatMap {
      case Encode(inputSymbols, _, _) => inputSymbols
      case _ => Seq.empty
    },
    codesBits = steps.flatMap(_.expectedBits),
    dictionarySizeAtTheEnd = dictionarySizeAtTheEnd,
  )
}

object SteppedEncodingFixture {
  sealed trait Step[Sym] {
    val expectedBits: Seq[String]
    val assertions: Seq[(LzwEncoder[Sym] => Any, Any)]
  }

  case class Encode[Sym](inputSymbols: Seq[Sym], expectedBits: Seq[String], assertions: Seq[(LzwEncoder[Sym] => Any, Any)]) extends Step[Sym]
  case class Reset[Sym](expectedBits: Seq[String], assertions: Seq[(LzwEncoder[Sym] => Any, Any)]) extends Step[Sym]
  case class Finish[Sym](expectedBits: Seq[String], assertions: Seq[(LzwEncoder[Sym] => Any, Any)]) extends Step[Sym]
}
