package predicateParsing

import org.scalatest.funspec.AnyFunSpec

class PredicateParserSpec extends AnyFunSpec {
  private case class Entity(string: String, int: Int)

  private val parser: PredicateParser[Entity] = new PredicateParser[Entity] {
    override protected val attributes: Seq[Attribute[_]] = Seq(
      Attribute("string", _.string),
      Attribute("int", _.int),
    )
  }

  private val parsingSuccessCases = Seq(
    """string == "name"""" -> true,
    """string != "name"""" -> false,
    "int == 7" -> true,
    "int != 7" -> false,
    "int < 7" -> false,
    "int > 7" -> false,
    "int <= 7" -> true,
    "int >= 7" -> true,
    "(int == 7)" -> true,
    "((int == 7))" -> true,
    """string == "name" && int == 7""" -> true,
    """string != "name" && int == 7""" -> false,
    """string == "name" && int != 7""" -> false,
    """string != "name" && int != 7""" -> false,
    """string == "name" || int == 7""" -> true,
    """string != "name" || int == 7""" -> true,
    """string == "name" || int != 7""" -> true,
    """string != "name" || int != 7""" -> false,
    """ string == "name" &&  int < 10  && int > 20 """ -> false,
    """ string == "name" ||  int < 10  || int > 20 """ -> true,
    """(string == "name" &&  int < 10) || int > 20 """ -> true,
    """ string == "name" && (int < 10  || int > 20)""" -> true,
    """(string == "name" ||  int < 10) && int > 20 """ -> false,
    """ string == "name" || (int < 10  && int > 20)""" -> true,
    """!(string == "name")""" -> false,
    """!(string != "name")""" -> true,
    """(!(string == "name"))""" -> false,
    """!(!(string == "name"))""" -> true,
    """!(string != "name") &&   int != 7 """ -> false,
    """  string != "name"  && !(int != 7)""" -> false,
    """!(string != "name") && !(int != 7)""" -> true,
  )

  private val entity = Entity("name", 7)
  for ((expression, expected) <- parsingSuccessCases) {
    describe(s"""The expression [$expression] as a predicate applied to $entity""") {
      it(s"results in $expected") {
        parser.parse(expression) match {
          case Left(message) => fail(s"Parsing failed: $message")
          case Right(predicate) => assertResult(expected)(predicate(entity))
        }
      }
    }
  }

  private val parsingFailureCases = Seq(
    "string",
    "long == 7",
    """string == "name" &&""",
    """string == "name" ||""",
    """string == "name" && int < 10 || int > 20""",
    """string == "name" || int < 10 && int > 20""",
  )

  for (expression <- parsingFailureCases) {
    describe(s"""Parsing the expression [$expression]""") {
      it("fails") {
        val result = parser.parse(expression)
        assert(result.isLeft)
        println(s"$expression: $result")
      }
    }
  }
}
