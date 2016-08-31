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

  def eval(expr: Expr, references: Map[String, Signal[Expr]], variableNamesSeen: Set[String] = Set()): Double = {
    expr match {
      case literal: Literal => literal.v
      case reference: Ref => {
        if (variableNamesSeen.contains(reference.name)) {
          Double.NaN
        }
        else if (references.contains(reference.name)) {
          eval(references(reference.name).apply(), references, variableNamesSeen + reference.name)
        }
        else {
          Double.NaN
        }
      }
      case plus: Plus => eval(plus.a, references, variableNamesSeen) + eval(plus.b, references, variableNamesSeen)
      case minus: Minus => eval(minus.a, references, variableNamesSeen) - eval(minus.b, references, variableNamesSeen)
      case times: Times => eval(times.a, references, variableNamesSeen) * eval(times.b, references, variableNamesSeen)
      case divide: Divide => {
        val denominator = eval(divide.b, references, variableNamesSeen)

        if (denominator == 0.0) {
          Double.NaN
        }
        else {
          eval(divide.a, references, variableNamesSeen) / denominator
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
