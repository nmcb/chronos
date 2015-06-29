package nmcb.chronos

import org.scalatest.{Matchers, FlatSpec}

class CalculatorTest extends FlatSpec with Matchers {
  
  "the calculator" should "parse operators" in {
    Calculator.parse("1*2") should be(BiOp("*", Val(1), Val(2)))
    Calculator.parse("1/2") should be(BiOp("/", Val(1), Val(2)))
    Calculator.parse("1+2") should be(BiOp("+", Val(1), Val(2)))
    Calculator.parse("1-2") should be(BiOp("-", Val(1), Val(2)))
  }

  "the calculator" should "parse values" in {
    Calculator.parse("0") should be(Val(0))
    Calculator.parse("1") should be(Val(1))
    Calculator.parse("0.1") should be(Val(0.1))
  }

  "the calculator" should "parse parentheses" in {
    Calculator.parse("(1+2)") should be(BiOp("+", Val(1), Val(2)))
  }

  it should "evaluate expressions correctly" in {
    Calculator.evaluate("(1+2+3)*(4-5-6)/(7+8+9)") should be(3)
  }
}
