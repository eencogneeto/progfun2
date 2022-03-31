package calculator

enum Expr:
  case Literal(v: Double)
  case Ref(name: String)
  case Plus(a: Expr, b: Expr)
  case Minus(a: Expr, b: Expr)
  case Times(a: Expr, b: Expr)
  case Divide(a: Expr, b: Expr)

object Calculator extends CalculatorInterface:
 import Expr.*

  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] =
    namedExpressions.map{
      case(name, x) => name -> Signal{
        eval(x(), namedExpressions)
      }
    }

  def eval(expr: Expr, references: Map[String, Signal[Expr]])(using Signal.Caller): Double =
    def evalInner(expr: Expr, prev: List[Expr]): Double = expr match
      case Literal(v) => v
      case Ref(name) if prev.contains(expr) => Double.NaN
      case Ref(name) => evalInner(getReferenceExpr(name, references), expr :: prev)
      case Plus(a, b) => evalInner(a, prev) + evalInner(b, prev)
      case Minus(a, b) => evalInner(a, prev) - evalInner(b, prev)
      case Times(a, b) => evalInner(a, prev) * evalInner(b, prev)
      case Divide(a, b) => evalInner(a, prev) / evalInner(b, prev)
    evalInner(expr, Nil)

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]])(using Signal.Caller): Expr =
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
