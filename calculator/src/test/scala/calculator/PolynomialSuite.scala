package calculator

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import org.scalatest._

@RunWith(classOf[JUnitRunner])
class PolynomialSuite extends FunSuite with ShouldMatchers {

  test("computeDelta with non-zero b") {
    val a = Var(0.0)
    val b = Var(3.0)
    val c = Var(0.0)

    val delta = Polynomial.computeDelta(a, b, c).apply()
    val expectedDelta = 9.0

    assert(delta === expectedDelta)
  }

  test("computeDelta with non-zero a and c") {
    val a = Var(2.0)
    val b = Var(0.0)
    val c = Var(3.0)

    val delta = Polynomial.computeDelta(a, b, c).apply()
    val expectedDelta = -24.0

    assert(delta === expectedDelta)
  }

  test("computeDelta with non-zero a, b and c") {
    val a = Var(2.0)
    val b = Var(5.0)
    val c = Var(3.0)

    val delta = Polynomial.computeDelta(a, b, c).apply()
    val expectedDelta = 1.0

    assert(delta === expectedDelta)
  }

  test("computeSolutions 0 roots when delta is less than 0") {
    val a = Var(2.0)
    val b = Var(5.0)
    val c = Var(3.0)
    val delta = Var(-1.0)

    val roots = Polynomial.computeSolutions(a, b, c, delta).apply()
    val expectedRoots = Set()

    assert(roots === expectedRoots)
  }

  test("computeSolutions 2 roots with delta of 1") {
    val a = Var(2.0)
    val b = Var(5.0)
    val c = Var(3.0)
    val delta = Polynomial.computeDelta(a, b, c) // 1

    val roots = Polynomial.computeSolutions(a, b, c, delta).apply()
    val expectedRoots = Set(-1.0, -1.5)

    assert(roots === expectedRoots)
  }

  test("computeSolutions 2 roots with delta greater than 1") {
    val a = Var(2.0)
    val b = Var(5.0)
    val c = Var(2.0)
    val delta = Polynomial.computeDelta(a, b, c) // 9

    val roots = Polynomial.computeSolutions(a, b, c, delta).apply()
    val expectedRoots = Set(-0.5, -2.0)

    assert(roots === expectedRoots)
  }

}
