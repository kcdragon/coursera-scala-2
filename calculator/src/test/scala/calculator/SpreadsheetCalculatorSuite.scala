package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

import TweetLength.MaxTweetLength

@RunWith(classOf[JUnitRunner])
class SpreadsheetCalculatorSuite extends FunSuite with ShouldMatchers {

  test("eval with a literal expression") {
    val expression = Literal(1.0)
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references) === 1.0)
  }

  test("eval with a plus of literals") {
    val expression = Plus(Literal(1.0), Literal(2.0))
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references) === 3.0)
  }

  test("eval with a minus of literals") {
    val expression = Minus(Literal(3.0), Literal(2.0))
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references) === 1.0)
  }

  test("eval with a times of literals") {
    val expression = Times(Literal(3.0), Literal(2.0))
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references) === 6.0)
  }

  test("eval with a divide of literals") {
    val expression = Divide(Literal(3.0), Literal(2.0))
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references) === 1.5)
  }

  test("eval with a divide and zero denominator") {
    val expression = Divide(Literal(1.0), Literal(0.0))
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references).equals(Double.NaN))
  }

  test("eval with a ref") {
    val expression = Ref("a")
    val references = Map[String, Signal[Expr]](
      "a" -> Var(Literal(1.0))
    )

    assert(Calculator.eval(expression, references) === 1.0)
  }

}