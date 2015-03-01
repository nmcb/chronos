package nmcb.chronos
import org.scalatest.{Matchers, FlatSpec}

class TimeTest extends FlatSpec with Matchers {

  "time" should "be ordered" in {
    Time(0,0) should be <= Time(0,0)
    Time(0,0) should be >= Time(0,0)
    Time(0,0) should be < Time(0,1)
    Time(0,0) should be < Time(1,0)
  }

  it should "be literally instantiable" in {
    Time("00:00") shouldBe  Time(0,0)
    Time("00:59") shouldBe  Time(0,59)
    Time("23:59") shouldBe  Time(23,59)
  }
}
