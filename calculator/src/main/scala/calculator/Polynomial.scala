package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Var(
      (b.apply() * b.apply()) -
        (4.0 * a.apply() * c.apply())
    )
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Var(
      if (delta.apply() < 0) Set()
      else {
        Set(
          (-1 * (b.apply()) + Math.sqrt(delta.apply())) / (2.0 * a.apply()),
          (-1 * (b.apply()) - Math.sqrt(delta.apply())) / (2.0 * a.apply())
        )
      }

    )
  }
}
