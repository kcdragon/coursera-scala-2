package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map {
      case (name, expression) => {
        name -> Var(eval(namedExpressions(name).apply(), namedExpressions))
      }
    }
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    expr match {
      case literal: Literal => literal.v
      case reference: Ref => {
        if (references.contains(reference.name)) {
          eval(references(reference.name).apply(), references)
        }
        else {
          Double.NaN
        }
      }
      case plus: Plus => eval(plus.a, references) + eval(plus.b, references)
      case minus: Minus => eval(minus.a, references) - eval(minus.b, references)
      case times: Times => eval(times.a, references) * eval(times.b, references)
      case divide: Divide => {
        val denominator = eval(divide.b, references)

        if (denominator == 0.0) {
          Double.NaN
        }
        else {
          eval(divide.a, references) / denominator
        }
      }
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
