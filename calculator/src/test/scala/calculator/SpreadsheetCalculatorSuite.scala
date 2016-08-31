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

  test("eval referencing an undefined variable is NaN") {
    val expression = Ref("a")
    val references = Map[String, Signal[Expr]]()

    assert(Calculator.eval(expression, references).equals(Double.NaN))
  }

  test("computeValues with a single reference to a literal") {
    val references = Map[String, Signal[Expr]](
      "a" -> Var(Literal(1.0))
    )

    val results = Calculator.computeValues(references)
    assert(results("a").apply() === 1.0)
  }

  test("computeValues with a reference to another") {
    val references = Map[String, Signal[Expr]](
      "a" -> Var(Ref("b")),
      "b" -> Var(Literal(1.0))
    )

    val results = Calculator.computeValues(references)
    assert(results("a").apply() === 1.0)
    assert(results("b").apply() === 1.0)
  }

  test("computeValues cyclic dependencies result in NaN") {
    val references = Map[String, Signal[Expr]](
      "a" -> Var(Ref("b")),
      "b" -> Var(Ref("a"))
    )

    val results = Calculator.computeValues(references)
    assert(results("a").apply().equals(Double.NaN))
    assert(results("b").apply().equals(Double.NaN))
  }

  test("computeValues cyclic dependencies in a subexpression result in NaN") {
    val references = Map[String, Signal[Expr]](
      "a" -> Var(Ref("b")),
      "b" -> Var(Plus(Ref("a"), Literal(1.0)))
    )

    val results = Calculator.computeValues(references)
    assert(results("a").apply().equals(Double.NaN))
    assert(results("b").apply().equals(Double.NaN))
  }

}
