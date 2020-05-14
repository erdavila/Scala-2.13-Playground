package hasSize

import hasSize.TraverserHasSize.Position

trait HasSize[C[_]] {
  def apply(c: C[_], expression: Expression): Boolean
}

object HasSize {
  implicit val listHasSize: HasSize[List] =
    followingTail[List] {
      case _ :: t => Some(t)
      case _ => None
    }

  implicit val lazyListHasSize: HasSize[LazyList] =
    followingTail[LazyList] {
      case _ +: t => Some(t)
      case _ => None
    }

  implicit val vectorDirectHasSize: HasSize[Vector] =
    direct[Vector](_.size)

  implicit class Ops[C[_]: HasSize](private val c: C[_]) {
    def hasSize = new Dsl(c)
  }

  class Dsl[C[_]](c: C[_])(implicit hasSize: HasSize[C]) {
    def ==(int: Int): Boolean = apply(_ == int)
    def !=(int: Int): Boolean = apply(_ != int)
    def <(int: Int): Boolean = apply(_ < int)
    def >(int: Int): Boolean = apply(_ > int)
    def <=(int: Int): Boolean = apply(_ <= int)
    def >=(int: Int): Boolean = apply(_ >= int)

    def apply(p: Size => Expression): Boolean = hasSize(c, p(Size))
  }

  def followingTail[C[_]](tailOption: C[_] => Option[C[_]]): TraverserHasSize[C] =
    new TraverserHasSize[C] {
      override def first(c: C[_]): Option[Position] =
        tailOption(c).map { t =>
          Position { first(t) }
        }
    }

  def iterating[C[_]](iterator: C[_] => Iterator[_]): TraverserHasSize[C] =
    TraverserHasSize[C] { c =>
      val it = iterator(c)
      def position(): Option[Position] =
        Option.when(it.hasNext) {
          it.next()
          Position { position() }
        }

      position()
    }

  def withState[C[_], S](first: C[_] => Option[S], next: S => Option[S]): TraverserHasSize[C] = {
    def position(s: S): Position =
      Position { next(s).map(position) }

    TraverserHasSize[C] { c =>
      first(c).map(position)
    }
  }

  def direct[C[_]](size: C[_] => Int): HasSize[C] =
    new HasSize[C] {
      override def apply(c: C[_], expression: Expression): Boolean =
        expression.evaluate(size(c)).value
    }
}
