package predicateParsing

import scala.util.parsing.combinator.JavaTokenParsers

trait BaseParsers extends JavaTokenParsers {
  object Literals {
    def int: Parser[Int] = wholeNumber ^? (
      { case AsInt(i) => i  },
      str => s""""$str"is not a valid Int """,
    )

    private object AsInt {
      def unapply(str: String): Option[Int] = str.toIntOption
    }

    def string: Parser[String] = stringLiteral ^^ { _.drop(1).dropRight(1) }
  }

  object Operations {
    def eq[T](operandParser: Parser[T]): Parser[T => Boolean] = "==" ~> operandParser ^^ { value => _ == value }
    def ne[T](operandParser: Parser[T]): Parser[T => Boolean] = "!=" ~> operandParser ^^ { value => _ != value }
    def lt[T: Numeric](operandParser: Parser[T]): Parser[T => Boolean] = numOp[T]("<", _.lt)(operandParser)
    def gt[T: Numeric](operandParser: Parser[T]): Parser[T => Boolean] = numOp[T](">", _.gt)(operandParser)
    def le[T: Numeric](operandParser: Parser[T]): Parser[T => Boolean] = numOp[T]("<=", _.lteq)(operandParser)
    def ge[T: Numeric](operandParser: Parser[T]): Parser[T => Boolean] = numOp[T](">=", _.gteq)(operandParser)

    private def numOp[T](token: String, op: Numeric[T] => (T, T) => Boolean)(operandParser: Parser[T])(implicit num: Numeric[T]): Parser[T => Boolean] =
      token ~> operandParser ^^ { value =>
        op(num)(_, value)
      }
  }
}
