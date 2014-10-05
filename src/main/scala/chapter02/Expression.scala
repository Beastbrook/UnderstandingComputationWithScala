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
  override def reduce: Expression = this match {
    case Add(left: Expression, right: Expression) if left.isReducible =>
      Add(left.reduce, right)
    case Add(left: Expression, right: Expression) if right.isReducible =>
      Add(left, right.reduce)
    case _: Expression =>
      Number(x.value + y.value)
  }
  override def toString: String = s"(${x} + ${y})"
}
case class Number(x: Int) extends Expression {
  override def isReducible: Boolean = false
  override def value: Int = x
  override def toString: String = x.toString
}
case class Multiply(x: Expression, y: Expression) extends Expression {
  override def reduce: Expression = this match {
    case Multiply(left: Expression, right: Expression) if left.isReducible =>
      Multiply(left.reduce, right)
    case Multiply(left: Expression, right: Expression) if right.isReducible =>
      Multiply(left, right.reduce)
    case _: Expression =>
      Number(x.value * y.value)
  }
  override def toString: String = s"${x} * ${y}"
}
case class Bool(v: Boolean) extends Expression {
  override def isReducible: Boolean = false
  override def toString: String = if (v) "true" else "false"
}