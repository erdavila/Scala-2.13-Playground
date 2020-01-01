package tset

import scala.annotation.unused
import scala.util.chaining._
import shapeless.test.illTyped
import tset.ops.IsSubsetOf

object TSetTest {
  def main(args: Array[String]): Unit = {
    testHas()
    testGet()
    testGetOption()
    testAdd()
    testReplace()
    testSet()
    testRemove()
    testUnset()
    testIsSubsetOf()
    testContains()
    testUnion()
    testIntersect()
    testDiff()
    testExpectedParameterType()
    testExpectedVariableType()
    println("OK")
  }

  private val s = TSet.empty.add(1).add("A")

  private def testHas(): Unit = {
    assert( s.has[Int])
    assert( s.has[String])
    assert(!s.has[Char])

    assert(!TSet.empty.has[Char])
  }

  private def testGet(): Unit = {
    assert(s.get[Int] == 1)
    assert(s.get[String] == "A")
    illTyped { """s.get[Char]""" }

    illTyped { """TSet.empty.get[Char]""" }
  }

  private def testGetOption(): Unit = {
    assert(s.getOption[Int] == Some(1))
    assert(s.getOption[String] == Some("A"))
    assert(s.getOption[Char] == None)

    assert(TSet.empty.getOption[Char] == None)
  }

  private def testAdd(): Unit = {
    val result = s.add('@')
    assert(result == TSet.empty.add(1).add('@').add("A"))

    illTyped { """s.add(1)""" }
    illTyped { """s.add("A")""" }
  }

  private def testReplace(): Unit = {
    {
      val result = s.replace(2)
      val expected = TSet.empty.add(2).add("A")
      assert(result == expected)

      assert(s.replaceGet(2) == ((expected, 1)))
    }

    {
      val result = s.replace("B")
      val expected = TSet.empty.add(1).add("B")
      assert(result == expected)

      assert(s.replaceGet("B") == ((expected, "A")))
    }

    illTyped { """s.replace('@')""" }
    illTyped { """s.replaceGet('@')""" }

    illTyped { """TSet.empty.replace('@')""" }
    illTyped { """TSet.empty.replaceGet('@')""" }
  }

  private def testSet(): Unit = {
    {
      val result = s.set(2)
      val expected = TSet.empty.add(2).add("A")
      assert(result == expected)

      assert(s.setGet(2) == ((expected, Some(1))))
    }

    {
      val result = s.set("B")
      val expected = TSet.empty.add(1).add("B")
      assert(result == expected)

      assert(s.setGet("B") == ((expected, Some("A"))))
    }

    {
      val result = s.set('@')
      val expected = TSet.empty.add(1).add('@').add("A")
      assert(result == expected)

      assert(s.setGet('@') == ((expected, None)))
    }

    {
      val result = TSet.empty.set('@')
      val expected = TSet.empty.add('@')
      assert(result == expected)

      assert(TSet.empty.setGet('@') == ((expected, None)))
    }
  }

  private def testRemove(): Unit = {
    {
      val result = s.remove[Int]()
      val expected = TSet.empty.add("A")
      assert(result == expected)

      assert(s.removeGet[Int]() == ((expected, 1)))
    }
    {
      val result = s.remove[String]()
      val expected = TSet.empty.add(1)
      assert(result == expected)

      assert(s.removeGet[String]() == ((expected, "A")))
    }

    illTyped { """s.remove[Char]()""" }
    illTyped { """s.removeGet[Char]()""" }

    illTyped { """TSet.empty.remove[Char]()""" }
    illTyped { """TSet.empty.removeGet[Char]()""" }
  }

  private def testUnset(): Unit = {
    {
      val result = s.unset[Int]()
      val expected = TSet.empty.add("A")
      assert(result == expected)

      assert(s.unsetGet[Int]() == ((expected, Some(1))))
    }

    {
      val result = s.unset[String]()
      val expected = TSet.empty.add(1)
      assert(result == expected)

      assert(s.unsetGet[String]() == ((expected, Some("A"))))
    }

    {
      val result = s.unset[Char]()
      val expected = TSet.empty.add("A").add(1)
      assert(result == expected)

      assert(s.unsetGet[Char]() == ((expected, None)))
    }

    {
      val result = TSet.empty.unset[Char]()
      val expected = TSet.empty
      assert(result == expected)

      assert(TSet.empty.unsetGet[Char]() == ((expected, None)))
    }
  }

  private object TypesOf {
    private[TSetTest] val ofS = TSet.typeOf(s)
    private[TSetTest] val i   = TSet.typeOf[tset.Empty].add[Int]()
    private[TSetTest] val is  = TSet.typeOf[tset.Empty].add[Int]().add[String]()
    private[TSetTest] val isc = TSet.typeOf[tset.Empty].add[Int]().add[String]().add[Char]()
    private[TSetTest] val ic  = TSet.typeOf[tset.Empty].add[Int]().add[Char]()
    private[TSetTest] val c   = TSet.typeOf[tset.Empty].add[Char]()
  }

  private def testIsSubsetOf(): Unit = {
    assert( s.isSubsetOf[TypesOf.ofS.T])
    assert(!s.isSubsetOf[TypesOf.i.T])
    assert( s.isSubsetOf[TypesOf.is.T])
    assert( s.isSubsetOf[TypesOf.isc.T])
    assert(!s.isSubsetOf[TypesOf.ic.T])
    assert(!s.isSubsetOf[TypesOf.c.T])
    assert(!s.isSubsetOf[tset.Empty])

    assert(TSet.empty.isSubsetOf[TypesOf.ofS.T])
    assert(TSet.empty.isSubsetOf[tset.Empty])
  }

  private def testContains(): Unit = {
    assert( s.contains[TypesOf.ofS.T])
    assert( s.contains[TypesOf.i.T])
    assert( s.contains[TypesOf.is.T])
    assert(!s.contains[TypesOf.isc.T])
    assert(!s.contains[TypesOf.ic.T])
    assert(!s.contains[TypesOf.c.T])
    assert( s.contains[tset.Empty])

    assert(!TSet.empty.contains[TypesOf.ofS.T])
    assert( TSet.empty.contains[tset.Empty])
  }

  private def testUnion(): Unit = {
    assert(s.union(s) == TSet.empty.add(1).add("A"))
    assert(s.union(TSet.empty.add(2)) == TSet.empty.add(2).add("A"))
    assert(s.union(TSet.empty.add("B")) == TSet.empty.add(1).add("B"))
    assert(s.union(TSet.empty.add(2).add("B")) == TSet.empty.add(2).add("B"))
    assert(s.union(TSet.empty.add('@')) == TSet.empty.add(1).add("A").add('@'))
    assert(s.union(TSet.empty.add(2).add('@')) == TSet.empty.add(2).add("A").add('@'))
    assert(s.union(TSet.empty) == TSet.empty.add(1).add("A"))

    assert(TSet.empty.union(s) == s)
    assert(TSet.empty.union(TSet.empty) == TSet.empty)
  }

  private def testIntersect(): Unit = {
    assert(s.intersect[TypesOf.ofS.T]() == TSet.empty.add(1).add("A"))
    assert(s.intersect[TypesOf.i.T]() == TSet.empty.add(1))
    assert(s.intersect[TypesOf.is.T]() == TSet.empty.add(1).add("A"))
    assert(s.intersect[TypesOf.isc.T]() == TSet.empty.add(1).add("A"))
    assert(s.intersect[TypesOf.ic.T]() == TSet.empty.add(1))
    assert(s.intersect[TypesOf.c.T]() == TSet.empty)
    assert(s.intersect[tset.Empty]() == TSet.empty)

    assert(TSet.empty.intersect[TypesOf.ofS.T]() == TSet.empty)
    assert(TSet.empty.intersect[tset.Empty]() == TSet.empty)
  }

  private def testDiff(): Unit = {
    assert(s.diff[TypesOf.ofS.T]() == TSet.empty)
    assert(s.diff[TypesOf.i.T]() == TSet.empty.add("A"))
    assert(s.diff[TypesOf.is.T]() == TSet.empty)
    assert(s.diff[TypesOf.isc.T]() == TSet.empty)
    assert(s.diff[TypesOf.ic.T]() == TSet.empty.add("A"))
    assert(s.diff[TypesOf.c.T]() == TSet.empty.add(1).add("A"))
    assert(s.diff[tset.Empty]() == TSet.empty.add(1).add("A"))

    assert(TSet.empty.diff[TypesOf.ofS.T]() == TSet.empty)
    assert(TSet.empty.diff[tset.Empty]() == TSet.empty)
  }

  private def testExpectedParameterType(): Unit = {
    val Expected = TSet.typeOf[tset.Empty].add[Int]().add[String]()
    def f[S <: TSet](param: S)(implicit @unused ev: Expected.T IsSubsetOf S): Unit = {}
    illTyped { """f(TSet.empty.add(1))""" }
    f(TSet.empty.add(1).add("A"))
    f(TSet.empty.add(1).add("A").add('@'))
  }

  private def testExpectedVariableType(): Unit = {
    val Expected = TSet.typeOf[tset.Empty].add[Int]().add[String]()

    @unused val vi0 = TSet.empty.add(1) `tap` { v =>
      @unused val typeOf = TSet.typeOf(v)
      illTyped { """implicitly[Expected.T IsSubsetOf typeOf.T]""" }
    }

    @unused val vis = TSet.empty.add(1).add("A") `tap` { v =>
      @unused val typeOf = TSet.typeOf(v)
      implicitly[Expected.T IsSubsetOf typeOf.T]
    }

    @unused val visc = TSet.empty.add(1).add("A").add('@') `tap` { v =>
      @unused val typeOf = TSet.typeOf(v)
      implicitly[Expected.T IsSubsetOf typeOf.T]
    }
  }
}
