import java.time.LocalDate
import org.scalatest.funspec.AnyFunSpec

class LensSpec extends AnyFunSpec {
  private case class Person(name: String, birth: LocalDate)

  private val localDate = LocalDate.of(1999, 12, 31)
  private val person = Person("John", birth = localDate)

  private val personBirth: Lens[Person, LocalDate] = Lens[Person](_.birth)(b => p => p.copy(birth = b))
  private val dateMonth: Lens[LocalDate, Int] = Lens[LocalDate](_.getMonthValue)(m => d => d.withMonth(m))
  private val personBirthMonth: Lens[Person, Int] = personBirth / dateMonth

  private val personBirthString: Lens[Person, String] = personBirth.transform(_.toString)(LocalDate.parse)
  private val tupleBirth: Lens[(String, LocalDate), LocalDate] = personBirth.cotransform(p => (p.name, p.birth))(Person.tupled)

  describe(".get") {
    it("can read a case class") {
      val result = personBirth.get(person)
      assertResult(localDate)(result)
    }

    it("it can read a regular class") {
      val result = dateMonth.get(localDate)
      assertResult(12)(result)
    }

    it("it can read nested data") {
      val result = personBirthMonth.get(person)
      assertResult(12)(result)
    }

    it("can transform the value") {
      val result = personBirthString.get(person)
      assertResult("1999-12-31")(result)
    }

    it("can co-transform the data") {
      val result = tupleBirth.get(("John", localDate))
      assertResult(localDate)(result)
    }
  }

  describe(".set") {
    it("can change a case class") {
      val result = personBirth.set(LocalDate.of(1997, 10, 27))(person)
      assertResult(Person("John", LocalDate.of(1997, 10, 27)))(result)
    }

    it("can change a regular class") {
      val result = dateMonth.set(10)(localDate)
      assertResult(LocalDate.of(1999, 10, 31))(result)
    }

    it("can change nested data") {
      val result = personBirthMonth.set(10)(person)
      assertResult(Person("John", LocalDate.of(1999, 10, 31)))(result)
    }

    it("can change the transformed value") {
      val result = personBirthString.set("1997-10-27")(person)
      assertResult(Person("John", LocalDate.of(1997, 10, 27)))(result)
    }

    it("can change a co-transformed data") {
      val result = tupleBirth.set(LocalDate.of(1997, 10, 27))(("John", localDate))
      assertResult(("John", LocalDate.of(1997, 10, 27)))(result)
    }
  }

  describe(".modify") {
    it("can modify a case class") {
      val result = personBirth.modify(_.minusMonths(2))(person)
      assertResult(Person("John", LocalDate.of(1999, 10, 31)))(result)
    }

    it("can modify a regular class") {
      val result = dateMonth.modify(_ - 2)(localDate)
      assertResult(LocalDate.of(1999, 10, 31))(result)
    }

    it("can modify nested data") {
      val result = personBirthMonth.modify(_ - 2)(person)
      assertResult(Person("John", LocalDate.of(1999, 10, 31)))(result)
    }

    it("can modify the transformed value") {
      val result = personBirthString.modify(_.replace("-12-", "-10-"))(person)
      assertResult(Person("John", LocalDate.of(1999, 10, 31)))(result)
    }

    it("can modify a co-transformed data") {
      val result = tupleBirth.modify(_.minusMonths(2))(("John", localDate))
      assertResult(("John", LocalDate.of(1999, 10, 31)))(result)
    }
  }
}
