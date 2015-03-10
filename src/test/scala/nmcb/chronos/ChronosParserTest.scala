package nmcb.chronos
import org.scalatest.{FlatSpec, Matchers}

class ChronosParserTest extends FlatSpec with Matchers {

  "chronos parser" should "period literals" in {
    ChronosParser.parse("Mo,Sa,Su 07:30-12:00,13:00-17:00") shouldBe
      Hours(
        List(Mo, Sa, Su),
        List(Period(Time(7, 30), Time(12, 0)), Period(Time(13,0), Time(17,0)) ))
  }
}
