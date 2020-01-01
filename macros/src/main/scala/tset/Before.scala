package tset

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

sealed class Before[A, B]

object Before {
  implicit def instance[A, B]: Before[A, B] =
    macro before[A, B]

  def before[A: c.WeakTypeTag, B: c.WeakTypeTag](c: whitebox.Context): c.Expr[Before[A, B]] = {
    import c.universe._
    val aType = weakTypeTag[A].tpe.dealias
    val bType = weakTypeTag[B].tpe.dealias
    val aName = aType.toString
    val bName = bType.toString
    if (aName < bName) {
      c.Expr(q"new Before[$aType, $bType]")
    } else {
      c.abort(c.enclosingPosition, "instance not available")
    }
  }
}
