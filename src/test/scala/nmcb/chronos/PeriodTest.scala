package nmcb.chronos
import org.scalatest.{FlatSpec, Matchers}

/**
 * A := []
 * B := ()
 *
 * ---[  ]--(  )---
 * ---[  (  ]  )---
 * ---[  (  )  ]---
 *
 * ---(  )--[  ]---
 * ---(  [  )  ]---
 * ---(  [  ]  )---
 */
class PeriodTest extends FlatSpec with Matchers {
  val t1 = Time("01:00")
  val t2 = Time("11:00")
  val t3 = Time("13:00")
  val t4 = Time("23:00")

  val a1 = Period(t1, t2)
  val a2 = Period(t1, t3)
  val a3 = Period(t1, t4)
  val a4 = Period(t3, t4)
  val a5 = Period(t2, t4)
  val a6 = Period(t2, t3)

  val b1 = Period(t3, t4)
  val b2 = Period(t2, t4)
  val b3 = Period(t2, t3)
  val b4 = Period(t1, t2)
  val b5 = Period(t1, t3)
  val b6 = Period(t1, t4)

  "a period" should "union another period" in {
    (a1 union b1) shouldBe (Right((Period(t1, t2), Period(t3, t4))))
    (a2 union b2) shouldBe (Left(Period(t1, t4)))
    (a3 union b3) shouldBe (Left(Period(t1, t4)))
    (a4 union b4) shouldBe (Right((Period(t1, t2), Period(t3, t4))))
    (a5 union b5) shouldBe (Left(Period(t1, t4)))
    (a6 union b6) shouldBe (Left(Period(t1, t4)))
  }

  it should "intersect another period" in {
    (a1 intersect b1) shouldBe (None)
    (a2 intersect b2) shouldBe (Some(Period(t2, t3)))
    (a3 intersect b3) shouldBe (Some(Period(t2, t3)))
    (a4 intersect b4) shouldBe (None)
    (a5 intersect b5) shouldBe (Some(Period(t2, t3)))
    (a6 intersect b6) shouldBe (Some(Period(t2, t3)))
  }

  it should "diff another period" in {
    (a1 diff b1) shouldBe (Some(Left(a1)))
    (a2 diff b2) shouldBe (Some(Left(Period(t1, t2))))
    (a3 diff b3) shouldBe (Some(Right((Period(t1, t2), Period(t3, t4)))))
    (a4 diff b4) shouldBe (Some(Left(a4)))
    (a5 diff b5) shouldBe (Some(Left(Period(t3, t4))))
    (a6 diff b6) shouldBe (None)
  }
}
