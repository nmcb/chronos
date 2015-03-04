package nmcb.chronos
import org.scalatest.{FlatSpec, Matchers}

class ChronosParserTest extends FlatSpec with Matchers {

  "chronos parser" should "period literals" in {
    ChronosParser.parse("Mo,Sa,Su 07:30-18:00") shouldBe Hours(List(Mo, Sa, Su), Period(Time(7, 30), Time(18, 0)))
  }
}
