package uc.expression

abstract class Expression()
object Expression {
  def isReducible(expression: Expression): Boolean = expression match {
    case _: Number => false
    case _: Expression => true
  }
}
case class Add(x: Expression, y: Expression) extends Expression
case class Number(x: Int) extends Expression
case class Multiply(x: Expression, y: Expression) extends Expression