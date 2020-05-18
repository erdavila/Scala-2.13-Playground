import java.time.temporal.ChronoUnit
import java.time.{ZoneId, ZonedDateTime}

object DaylightSavingTime {
  def main(args: Array[String]): Unit = {
    val zone = ZoneId.of("America/Sao_Paulo")
    showTimeAroundChange("Daylight Saving Time begin", ZonedDateTime.of(2018, 11, 3, 22, 0, 0, 0, zone))
    println()
    showTimeAroundChange("Daylight Saving Time end", ZonedDateTime.of(2019, 2, 16, 22, 0, 0, 0, zone))
  }

  private def showTimeAroundChange(description: String, zonedDateTimeBeforeChange: ZonedDateTime): Unit = {
    println(description)

    for (hoursDelta <- 0 until 4) {
      val zonedDateTime = zonedDateTimeBeforeChange.plus(hoursDelta, ChronoUnit.HOURS)
      val offsetDateTime = zonedDateTime.toOffsetDateTime
      val values = Seq(
        "instant" -> zonedDateTime.toInstant,
        "zonedDateTime" -> zonedDateTime,
        "offsetDateTime" -> offsetDateTime,
        "offset" -> offsetDateTime.getOffset,
      )
      println(values.map { case (label, value) => s"$label = $value" }.mkString("   "))
    }
  }
}
