// http://www.scalatest.org/user_guide/sharing_fixtures#getFixtureMethods
package scalatestFixtures

import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ListBuffer
import scala.language.reflectiveCalls

class CallingGetFixtureMethods extends AnyFlatSpec {
  private def fixture =
    new {
      val builder = new StringBuilder("ScalaTest is ")
      val buffer = new ListBuffer[String]
    }

  "Testing" should "be easy" in {
    val f = fixture
    f.builder.append("easy!")
    assert(f.builder.toString === "ScalaTest is easy!")
    assert(f.buffer.isEmpty)
    f.buffer += "sweet"
  }

  it should "be fun" in {
    val f = fixture
    f.builder.append("fun!")
    assert(f.builder.toString === "ScalaTest is fun!")
    assert(f.buffer.isEmpty)
  }
}
