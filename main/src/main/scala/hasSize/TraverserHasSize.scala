package hasSize

import hasSize.TraverserHasSize.Position
import scala.annotation.tailrec

trait TraverserHasSize[C[_]] extends HasSize[C] {
  def first(c: C[_]): Option[Position]

  final override def apply(c: C[_], expression: Expression): Boolean =
    TraverserHasSize.traverse(first(c), expression)
}

object TraverserHasSize {
  def apply[C[_]](first: C[_] => Option[Position]): TraverserHasSize[C] = {
    val f = (c: C[_]) => first(c)
    new TraverserHasSize[C] {
      override def first(c: C[_]): Option[Position] = f(c)
    }
  }

  trait Position {
    def next(): Option[Position]
  }

  object Position {
    def apply(next: => Option[Position]): Position = {
      val n = () => next
      new Position() {
        override def next(): Option[Position] = n()
      }
    }
  }

  def traverse(firstPosition: Option[Position], expression: Expression): Boolean = {
    import Expression.Evaluation._

    @tailrec
    def loop(leastKnownSize: Int, nextPosition: => Option[Position]): Boolean =
      expression.evaluate(leastKnownSize) match {
        case Definitive(value) => value
        case Provisional(value) =>
          nextPosition match {
            case Some(position) => loop(leastKnownSize + 1, position.next())
            case None => value
          }
      }

    loop(0, firstPosition)
  }
}
