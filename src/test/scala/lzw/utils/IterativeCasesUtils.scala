package lzw.utils

import lzw.core.Code

object IterativeCasesUtils {

  def specialCodesIterator(alphabetSize: Int): Iterator[(Option[Code], Option[Code])] = {
    def single: Iterator[Code] = Iterator(
      0,                // !ss⋯s__⋯
      1,                // s!s⋯s__⋯
      alphabetSize,     // ss⋯s!__⋯
      alphabetSize + 1, // ss⋯s_!_⋯
    )

    def both: Iterator[(Code, Code)] =
      Iterator(
        (0, 1),                               // !!sss⋯s___⋯
        (0, 2),                               // !s!ss⋯s___⋯
        (0, alphabetSize + 1),                // !sss⋯s!___⋯
        (0, alphabetSize + 2),                // !sss⋯s_!__⋯
        (1, 2),                               // s!!ss⋯s___⋯
        (1, 3),                               // s!s!s⋯s___⋯
        (1, alphabetSize + 1),                // s!ss⋯s!___⋯
        (1, alphabetSize + 2),                // s!ss⋯s_!__⋯
        (alphabetSize, alphabetSize + 1),     // sss⋯s!!___⋯
        (alphabetSize, alphabetSize + 2),     // sss⋯s!_!__⋯
        (alphabetSize + 1, alphabetSize + 2), // sss⋯s_!!__⋯
        (alphabetSize + 1, alphabetSize + 3), // sss⋯s_!_!_⋯
      )

    (
      Iterator((None, None)) ++
        (for (c <- single) yield (Some(c), None)) ++
        (for (c <- single) yield (None, Some(c))) ++
        (for ((c1, c2) <- both) yield (Some(c1), Some(c2))) ++
        (for ((c1, c2) <- both) yield (Some(c2), Some(c1)))
      ).distinct
  }

  def split[Sym](symbols: Seq[Sym], parts: Int): Seq[Seq[Sym]] = {
    require(parts >= 1)
    if (parts == 1) {
      Seq(symbols)
    } else {
      val (part, rest) = symbols.splitAt(symbols.size / parts)
      part +: split(rest, parts - 1)
    }
  }

  def withCase[T](index: Int, options: Any, additional: (String, Any))(fun: => T): T =
    try {
      fun
    } catch {
      case t: Throwable =>
        val message =
          s"""${t.getMessage}
             |  index = $index
             |  options = $options
             |  ${additional._1} = ${additional._2}
             |""".stripMargin.stripLineEnd

        throw new Exception(message, t)
    }
}
