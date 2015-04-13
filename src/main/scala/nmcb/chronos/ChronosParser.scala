package nmcb.chronos

import org.parboiled.errors
import org.parboiled.errors.ErrorUtils
import org.parboiled.scala.Parser
import org.parboiled.scala.parserunners.ReportingParseRunner

object ChronosParser extends Parser {

  def parse(str: String): Chronos = {
    val parsingResult = ReportingParseRunner(HOURS).run(str)
    parsingResult.result match {
      case Some(i) => i
      case None    => {
        val msg = ErrorUtils.printParseErrors(parsingResult)
        throw new errors.ParsingException("invalid input: " + msg)
      }
    }
  }

  val HOURS = rule {
    zeroOrMore(DAYS, separator = ",") ~ WS ~ oneOrMore(PERIOD, separator = ",") ~~> Hours
  }

  val DAYS = rule { MON | TUE | WED | THU | FRI | SAT | SUN }

  val MON = rule { optional(WS) ~ "Mo" ~ push(Mo) }
  val TUE = rule { optional(WS) ~ "Tu" ~ push(Tu) }
  val WED = rule { optional(WS) ~ "We" ~ push(We) }
  val THU = rule { optional(WS) ~ "Th" ~ push(Th) }
  val FRI = rule { optional(WS) ~ "Fr" ~ push(Fr) }
  val SAT = rule { optional(WS) ~ "Sa" ~ push(Sa) }
  val SUN = rule { optional(WS) ~ "Su" ~ push(Su) }

  val PERIOD = rule { TIME ~ "-" ~ TIME ~~> Period ~ WS }
  val TIME   = rule { nTimes(1, PAIR ~ ":" ~ PAIR) ~> (s => Time(s)) }
  val PAIR   = rule { DIGIT ~ DIGIT }
  val DIGIT  = rule { "0" - "9" }
  val WS     = rule { zeroOrMore(anyOf(" \n\r\t\f")) }
}