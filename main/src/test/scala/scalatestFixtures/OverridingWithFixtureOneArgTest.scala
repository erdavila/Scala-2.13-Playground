// http://www.scalatest.org/user_guide/sharing_fixtures#withFixtureOneArgTest
package scalatestFixtures

import java.io.{File, FileWriter}
import org.scalatest.Outcome
import org.scalatest.flatspec.FixtureAnyFlatSpec

class OverridingWithFixtureOneArgTest extends FixtureAnyFlatSpec {
  case class FixtureParam(file: File, writer: FileWriter)

  protected override def withFixture(test: OneArgTest): Outcome = {
    val file = File.createTempFile("hello", "world") // create the fixture
    val writer = new FileWriter(file)
    val theFixture = FixtureParam(file, writer)

    try {
      writer.write("ScalaTest is ") // set up the fixture
      withFixture(test.toNoArgTest(theFixture)) // "loan" the fixture to the test
    }
    finally writer.close() // clean up the fixture
  }

  "Testing" should "be easy" in { f =>
    f.writer.write("easy!")
    f.writer.flush()
    assert(f.file.length === 18)
  }

  it should "be fun" in { f =>
    f.writer.write("fun!")
    f.writer.flush()
    assert(f.file.length === 17)
  }
}
