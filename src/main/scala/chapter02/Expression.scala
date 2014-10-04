package uc.expression

abstract class Expression() {
  def isReducible: Boolean = true
  def reduce: Expression = this
  def value: Int = this.reduce.value
}
object Expression {
  def isReducible(expression: Expression): Boolean = expression match {
    case _: Number => false
    case _: Expression => true
  }
}
case class Add(x: Expression, y: Expression) extends Expression {
  override def reduce: Expression = Number(x.reduce.value + y.reduce.value)
}
case class Number(x: Int) extends Expression {
  override def isReducible: Boolean = false
  override def value: Int = x
}
case class Multiply(x: Expression, y: Expression) extends Expression {
  override def reduce: Expression = Number(x.reduce.value * y.reduce.value)
}