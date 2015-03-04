package nmcb.chronos
import org.scalatest.{FlatSpec, Matchers}

class DayTest extends FlatSpec with Matchers {

  "days" should "be ordered" in {
    // why is Day not inferred?
    List[Day](Su, Sa, Fr, Th, We, Tu, Mo).sorted shouldBe List(Mo, Tu, We, Th, Fr, Sa, Su)
  }
}
