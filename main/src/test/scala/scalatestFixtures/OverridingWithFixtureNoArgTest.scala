// http://www.scalatest.org/user_guide/sharing_fixtures#withFixtureNoArgTest
package scalatestFixtures

import java.io.{File, FileWriter}
import org.scalatest.Outcome
import org.scalatest.flatspec.AnyFlatSpec

class OverridingWithFixtureNoArgTest extends AnyFlatSpec {
  var fileName: String = _

  override def withFixture(test: NoArgTest): Outcome = {
    val file = File.createTempFile("hello", "world")
    val writer = new FileWriter(file)

    try {
      writer.write("ScalaTest is ")
      writer.flush()
      writer.close()
      fileName = file.getPath
      super.withFixture(test)
    } finally {
      // Perform cleanup
    }
  }

  "Testing" should "be easy" in {
    val file = new File(fileName)
    val writer = new FileWriter(file, true)
    writer.write("easy!")
    writer.flush()
    assert(file.length === 18)
  }

  it should "be fun" in {
    val file = new File(fileName)
    val writer = new FileWriter(file, true)
    writer.write("fun!")
    writer.flush()
    assert(file.length === 17)
  }
}
