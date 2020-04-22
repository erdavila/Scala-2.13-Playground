// http://www.scalatest.org/user_guide/sharing_fixtures#fixtureContextObjects
package scalatestFixtures

import org.scalatest.flatspec.AnyFlatSpec
import scala.collection.mutable.ListBuffer

class InstantiatingFixtureContextObjects extends AnyFlatSpec {
  private trait Builder {
    val builder = new StringBuilder("ScalaTest is ")
  }

  private trait Buffer {
    val buffer: ListBuffer[String] = ListBuffer("ScalaTest", "is")
  }

  // This test needs the StringBuilder fixture
  "Testing" should "be productive" in new Builder {
    builder.append("productive!")
    assert(builder.toString === "ScalaTest is productive!")
  }

  // This test needs the ListBuffer[String] fixture
  "Test code" should "be readable" in new Buffer {
    buffer += ("readable!")
    assert(buffer === List("ScalaTest", "is", "readable!"))
  }

  // This test needs both the StringBuilder and ListBuffer
  it should "be clear and concise" in new Builder with Buffer {
    builder.append("clear!")
    buffer += ("concise!")
    assert(builder.toString === "ScalaTest is clear!")
    assert(buffer === List("ScalaTest", "is", "concise!"))
  }
}
