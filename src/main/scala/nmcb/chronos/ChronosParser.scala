package nmcb.chronos
import org.parboiled.errors
import org.parboiled.errors.ErrorUtils
import org.parboiled.scala.Parser
import org.parboiled.scala.parserunners.ReportingParseRunner

object ChronosParser extends Parser {

  def HOURS = rule {
    zeroOrMore(DAYS, separator = ",") ~ WS ~ oneOrMore(PERIOD, separator = ",") ~~> Hours
  }

  def DAYS = rule { MON | TUE | WED | THU | FRI | SAT | SUN }

  def MON = rule { optional(WS) ~ "Mo" ~ push(Mo) }
  def TUE = rule { optional(WS) ~ "Tu" ~ push(Tu) }
  def WED = rule { optional(WS) ~ "We" ~ push(We) }
  def THU = rule { optional(WS) ~ "Th" ~ push(Th) }
  def FRI = rule { optional(WS) ~ "Fr" ~ push(Fr) }
  def SAT = rule { optional(WS) ~ "Sa" ~ push(Sa) }
  def SUN = rule { optional(WS) ~ "Su" ~ push(Su) }

  def PERIOD = rule { TIME ~ "-" ~ TIME ~~> Period ~ WS }
  def TIME = rule { nTimes(1, PAIR ~ ":" ~ PAIR) ~> (s => Time(s)) }
  def PAIR = rule { DIGIT ~ DIGIT }
  def DIGIT = rule { "0" - "9" }

  def WS = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

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
}