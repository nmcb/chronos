package nmcb.chronos

sealed abstract class Chronos

case class Time(hour: Int, minute: Int) extends Chronos with Ordered[Time] {
  val unit = 60
  val value = hour * unit + minute
  override def compare(that: Time) = value - that.value
}

object Time {
  import java.time.format.DateTimeFormatter._
  import java.time.temporal.ChronoField._
  def apply(str: String): Time = {
    val acc = ISO_LOCAL_TIME.parse(str)
    Time(acc.get(HOUR_OF_DAY), acc.get(MINUTE_OF_HOUR))
  }
}

case class Period(from: Time, till: Time) extends Chronos {
  def union(that: Period): Either[Period, (Period, Period)] =
    if (from < that.from) {
      if (till < that.from) Right((this, that))
      else if (till < that.till) Left(Period(from, that.till))
      else Left(this)
    }
    else {
      if (that.till < from) Right((that, this))
      else if (that.till < till) Left(Period(that.from, till))
      else Left(that)
    }

  def intersection(that: Period): Option[Period] =
    if (from < that.from) {
      if (till < that.from) None
      else if (till < that.till) Some(Period(that.from, till))
      else Some(that)
    }
    else {
      if (that.till < from) None
      else if (that.till < till) Some(Period(from, that.till))
      else Some(this)
    }

  def diff(that: Period): Option[Either[Period, (Period, Period)]] =
    if (from < that.from) {
      if (till < that.from) Some(Left(this))
      else if (till < that.till) Some(Left(Period(from, that.from)))
      else Some(Right((Period(from, that.from), Period(that.till, till))))
    }
    else {
      if (that.till < from) Some(Left(this))
      else if (that.till < till) Some(Left(Period(that.till, till)))
      else None
    }
}

abstract class Day extends Chronos with Ordered[Day] {
  import nmcb.chronos.Day._
  override def compare(that: Day) = NaturalOrdering.compare(this, that)
}

case object Mo extends Day
case object Tu extends Day
case object We extends Day
case object Th extends Day
case object Fr extends Day
case object Sa extends Day
case object Su extends Day

object Day {
  val fromInt: Int => Day =
    Array(Mo, Tu, We, Th, Fr, Sa, Su)

  val toInt: Day => Int =
    Map(Mo -> 0, Tu -> 1, We -> 2, Th -> 3, Fr -> 4, Sa -> 5, Su -> 6)

  implicit val NaturalOrdering = new Ordering[Day] {
    override def compare(x: Day, y: Day) = toInt(x) compare toInt(y)
  }
}

case class Hours(days: List[Day], periods: List[Period]) extends Chronos


