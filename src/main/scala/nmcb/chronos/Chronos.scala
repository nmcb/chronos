package nmcb.chronos
import java.text.SimpleDateFormat
import java.time.format.DateTimeFormatter
import java.time.temporal.{TemporalAccessor, ChronoField}

sealed abstract class Chronos

case class Time(hour: Int, minute: Int) extends Chronos with Ordered[Time] {
  val unit = 60
  override def compare(that: Time) =
    (hour * unit + minute) - (that.hour * unit + that.minute)
}

object Time {
  val format = DateTimeFormatter.ISO_LOCAL_TIME
  def apply(str: String): Time = {
    val acc = format.parse(str)
    Time(acc.get(ChronoField.HOUR_OF_DAY), acc.get(ChronoField.MINUTE_OF_HOUR))
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

  def intersect(that: Period): Option[Period] =
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

abstract class Day extends Chronos
case object Ma extends Day
case object Tu extends Day
case object We extends Day
case object Th extends Day
case object Fr extends Day
case object Sa extends Day
case object Su extends Day

case class Hours(periods: Map[Day, List[Period]])


