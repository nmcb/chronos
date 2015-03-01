package nmcb.chronos
import org.scalatest.{FlatSpec, Matchers}

class ChronosParserTest extends FlatSpec with Matchers {

  "chronos parser" should "period literals" in {
    //    ChronosParser.parse("00:00-01:00") shouldBe Period(Time("00:00"), Time("01:00"))
    ChronosParser.parse("Mo, Sa, Su 07:30-12:00") shouldBe Hours(List(Mo, Sa, Su), Period(Time(7, 30), Time(12, 0)))
  }
}
