package predicateParsing

abstract class PredicateParser[T](
  andToken: String = "&&",
  orToken: String = "||",
  notToken: String = "!",
) {
  case class Attribute[A](name: String, access: T => A)(implicit val operation: Operation[A])

  protected val attributes: Seq[Attribute[_]]

  private type Predicate = T => Boolean

  private object parsers extends BaseParsers {
    def predicate: Parser[Predicate] = and | or | simple
    def simple: Parser[Predicate] = test | delimited | not
    def delimited: Parser[Predicate] = "(" ~> predicate <~ ")"

    def and: Parser[Predicate] = simple ~ andToken ~ (and | simple) ^^ { case p1 ~ _ ~ p2 => t => p1(t) && p2(t) }
    def or: Parser[Predicate] = simple ~ orToken ~ (or | simple) ^^ { case p1 ~ _ ~ p2 => t => p1(t) || p2(t) }
    def not: Parser[Predicate] = notToken ~> delimited ^^ { p => t => !p(t) }

    def test: Parser[Predicate] = ident.into { attrName =>
      attributes.find(_.name == attrName) match {
        case Some(attribute) => operationParser(attribute)
        case None => failure(s"unknown attribute $attrName")
      }
    }

    private def operationParser[A](attribute: Attribute[A]): Parser[Predicate] =
      attribute.operation.parser(this) ^^ { attribute.access `andThen` _ }
  }

  def parse(expression: String): Either[String, T => Boolean] = {
    import parsers._
    parseAll(predicate, expression) match {
      case Success(result, _) => Right(result)
      case ns: NoSuccess => Left(ns.msg)
    }
  }
}
