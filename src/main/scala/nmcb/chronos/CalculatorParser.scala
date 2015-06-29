package nmcb.chronos

import org.parboiled.errors._
import org.parboiled.scala._
import org.parboiled.scala.parserunners.ReportingParseRunner
import org.parboiled.scala.rules.Rule1

abstract class Expr

case class Val(value: Double) extends Expr
case class BiOp(operator: String, lhs: Expr, rhs: Expr) extends Expr

object CalculatorParser extends Parser {
  type R0 = Rule0
  type R1[A] = Rule1[A]

  def Expr: R1[Expr] = rule {
    Term ~ zeroOrMore(
      "+" ~ Term ~~> ((lhs: Expr, rhs: Expr) => BiOp("+", lhs, rhs)) |
      "-" ~ Term ~~> ((lhs: Expr, rhs: Expr) => BiOp("-", lhs, rhs))
    )
  }

  def Term: R1[Expr] = rule {
    Factor ~ zeroOrMore(
      "*" ~ Factor ~~> ((lhs: Expr, rhs: Expr) => BiOp("*", lhs, rhs)) |
      "/" ~ Factor ~~> ((lhs: Expr, rhs: Expr) => BiOp("/", lhs, rhs))
    )
  }

  def Factor: R1[Expr] = rule { "(" ~ Expr ~ ")" | Number }
  def Number: R1[Val]  = rule { group(Integer ~ optional(Fraction)) ~> (s => Val(s.toDouble)) ~ WS }
  def Fraction         = rule { "." ~ Digits }
  def Integer          = rule { optional("-") ~ (("1" - "9") ~ Digits | Digit) }
  def Digits           = rule { oneOrMore(Digit) }
  def Digit            = rule { "0" - "9" }
  def WS               = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  def parse(str: String): Expr = {
    val parsingResult = ReportingParseRunner(Expr).run(str)
    parsingResult.result match {
      case Some(i) => i
      case None    => {
        val msg = ErrorUtils.printParseErrors(parsingResult)
        throw new ParsingException("invalid input: " + msg)
      }
    }
  }
  
  def evaluate(expr: Expr): Double = expr match {
    case Val(value) => value
    case BiOp("*", lhs, rhs) => evaluate(lhs) * evaluate(rhs)
    case BiOp("/", lhs, rhs) => evaluate(lhs) / evaluate(rhs)
    case BiOp("+", lhs, rhs) => evaluate(lhs) + evaluate(rhs)
    case BiOp("-", lhs, rhs) => evaluate(lhs) - evaluate(rhs)
  }
  
  def evaluate(str: String): Double = evaluate(parse(str))
}
