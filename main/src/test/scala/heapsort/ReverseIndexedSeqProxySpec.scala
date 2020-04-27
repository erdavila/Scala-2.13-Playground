package heapsort

import org.scalatest.Outcome
import org.scalatest.funspec.FixtureAnyFunSpec

class ReverseIndexedSeqProxySpec extends FixtureAnyFunSpec {

  case class FixtureParam(array: Array[Char], risp: ReverseIndexedSeqProxy[Char])

  override def withFixture(test: OneArgTest): Outcome = {
    val array = ('A' to 'E').toArray
    val risp = ReverseIndexedSeqProxy(array)
    val theFixture = FixtureParam(array, risp)
    withFixture(test.toNoArgTest(theFixture))
  }

  describe("elements") {
    they("are correct") { f =>
      assertResult(f.array.reverse)(f.risp)
    }
  }

  describe(".apply()") {
    it("translates to the underlying sequence") { f =>
      assertResult('D')(f.risp(1))
      assertResult('B')(f.risp(3))
    }
  }

  describe(".update()") {
    it("changes the underlying sequence") { f =>
      f.risp(1) = '!'
      f.risp(3) = '?'

      assertResult(Seq('A', '?', 'C', '!', 'E'))(f.array)
    }
  }
}
