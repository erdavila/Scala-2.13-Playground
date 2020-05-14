package hasSize

sealed trait Expression {
  import hasSize.Expression._

  def evaluate(leastKnownSize: Int): Evaluation

  def &&(that: Expression): Expression = And(this, that)
  def ||(that: Expression): Expression = Not(And(Not(this), Not(that)))
  def unary_! : Expression = Not(this)
}

object Expression {
  import Evaluation._

  case class Lt(int: Int) extends Expression {
    override def evaluate(leastKnownSize: Int): Evaluation =
      if (leastKnownSize < int) {
        Provisional(true)
      } else {
        Definitive(false)
      }
  }

  case class Not(expr: Expression) extends Expression {
    override def evaluate(leastKnownSize: Int): Evaluation =
      expr.evaluate(leastKnownSize) match {
        case Definitive(value) => Definitive(!value)
        case Provisional(value) => Provisional(!value)
      }
  }

  object Not {
    def apply(expr: Expression): Expression =
      expr match {
        case Not(e) => e
        case _ => new Not(expr)
      }
  }

  case class And(expr1: Expression, expr2: Expression) extends Expression {
    override def evaluate(leastKnownSize: Int): Evaluation =
      (expr1.evaluate(leastKnownSize), expr2.evaluate(leastKnownSize)) match {
        case (Provisional(value1), Provisional(value2)) => Provisional(value1 && value2)
        case (Definitive(false), _) | (_, Definitive(false)) => Definitive(false)
        case (Definitive(true), ev2) => ev2
        case (ev1, Definitive(true)) => ev1
      }
  }

  sealed trait Evaluation {
    val value: Boolean
  }

  object Evaluation {
    case class Definitive(value: Boolean) extends Evaluation
    case class Provisional(value: Boolean) extends Evaluation
  }
}
