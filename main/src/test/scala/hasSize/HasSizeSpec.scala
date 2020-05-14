package hasSize

import hasSize.HasSize.Ops
import org.scalatest.funspec.AnyFunSpec

class HasSizeSpec extends AnyFunSpec {
  def testWith[C[_]: HasSize](typeName: String, make: Int => C[_]): Unit = {
    val cases = Seq[(String, C[_] => Boolean, Boolean, Boolean, Boolean, Boolean)](
      ("hasSize == 5", _.hasSize == 5, false, false, true, false),
      ("hasSize != 5", _.hasSize != 5, true, true, false, true),
      ("hasSize < 5", _.hasSize < 5, true, true, false, false),
      ("hasSize > 5", _.hasSize > 5, false, false, false, true),
      ("hasSize <= 5", _.hasSize <= 5, true, true, true, false),
      ("hasSize >= 5", _.hasSize >= 5, false, false, true, true),

      ("hasSize(s => s == 5)", _.hasSize(s => s == 5), false, false, true, false),
      ("hasSize(s => s != 5)", _.hasSize(s => s != 5), true, true, false, true),
      ("hasSize(s => !(s == 5))", _.hasSize(s => !(s == 5)), true, true, false, true),
      ("hasSize(s => s > 4 && s < 6)", _.hasSize(s => s > 4 && s < 6), false, false, true, false),
      ("hasSize(s => s == 4 || s == 6)", _.hasSize(s => s == 4 || s == 6), false, true, false, true),
      ("hasSize(s => s == 4 || (s > 5 && !(s > 6)))", _.hasSize(s => s == 4 || (s > 5 && !(s > 6))), false, true, false, true),
    )

    describe(typeName) {
      for ((description, eval, expected0, expected4, expected5, expected6) <- cases) {
        describe(description) {
          for ((expected, size) <- Seq((expected0, 0), (expected4, 4), (expected5, 5), (expected6, 6))) {
            it(s"evaluates to $expected when length == $size") {
              val c = make(size)
              assertResult(expected)(eval(c))
            }
          }
        }
      }
    }
  }

  describe("List") {
    it should behave like testWith("with default HasSize", List.range(0, _))

    it should behave like testWith("with HasSize.followingTail", List.range(0, _))(
      HasSize.followingTail[List] {
        case _ +: t => Some(t)
        case _ => None
      }
    )

    it should behave like testWith("with HasSize.iterating", List.range(0, _))(
      HasSize.iterating[List](_.iterator)
    )
  }

  describe("Vector") {
    it should behave like testWith("with default HasSize", Vector.range(0, _))

    it should behave like testWith("with HasSize.withState", Vector.range(0, _))(
      HasSize.withState[Vector, (Int, Int)](
        c => Option.when(c.nonEmpty)((0, c.size)),
        { case (i, size) => Option.when(i < size - 1)((i + 1, size)) },
      )
    )

    it should behave like testWith("with HasSize.direct", Vector.range(0, _))(
      HasSize.direct[Vector](_.size)
    )
  }

  describe("LazyList with infinite size") {
    case class CounterRef(var value: Int)
    def f(): (LazyList[_], CounterRef) = {
      val ref = CounterRef(0)
      val c = LazyList.continually {
        val n = ref.value
        ref.value += 1
        n
      }
      (c, ref)
    }

    val cases = Seq[(String, LazyList[_] => Boolean, Boolean, Int)](
      ("hasSize == 5", _.hasSize == 5, false, 6),
      ("hasSize != 5", _.hasSize != 5, true, 6),
      ("hasSize > 5", _.hasSize > 5, true, 6),
      ("hasSize < 5", _.hasSize < 5, false, 5),
      ("hasSize >= 5", _.hasSize >= 5, true, 5),
      ("hasSize <= 5", _.hasSize <= 5, false, 6),
    )

    for ((description, eval, expected, steps) <- cases) {
      describe(description) {
        it(s"evaluates to $expected") {
          val (c, ref) = f()
          assertResult(expected)(eval(c))
          assertResult(steps)(ref.value)
        }
      }
    }
  }
}
