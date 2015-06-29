package nmcb.chronos

import org.scalatest.{Matchers, FlatSpec}

class CalculatorParserTest extends FlatSpec with Matchers {
  
  "the parser" should "parse expression strings" in {
    CalculatorParser.parse("1*2") should be(BiOp("*", Val(1), Val(2)))
    CalculatorParser.parse("1/2") should be(BiOp("/", Val(1), Val(2)))
    CalculatorParser.parse("1+2") should be(BiOp("+", Val(1), Val(2)))
    CalculatorParser.parse("1-2") should be(BiOp("-", Val(1), Val(2)))

    CalculatorParser.parse("0") should be(Val(0))
    CalculatorParser.parse("1") should be(Val(1))
    CalculatorParser.parse("0.1") should be(Val(0.1))

    CalculatorParser.parse("(1+2)") should be(BiOp("+", Val(1), Val(2)))
  }
  
  it should "evaluate expressions" in {
    CalculatorParser.evaluate("1+2") should be(3)
  }
}
