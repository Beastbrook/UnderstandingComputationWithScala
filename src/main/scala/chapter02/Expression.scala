package uc.simple

trait Expression {
  def isReducible: Boolean = true
  def reduce(env: Map[String, Expression]): Expression = this
  def reduced(env: Map[String, Expression]): Expression = {
    def go(exp: Expression): Expression = exp match {
      case e: Expression if !(e.isReducible) => e
      case e: Expression => go(e.reduce(env))
    }
    go(this)
  }
  def value(env: Map[String, Expression]): Int = this.reduce(env).value(env)
}
object Expression {
  def isReducible(expression: Expression): Boolean = expression match {
    case _: Number => false
    case _: Expression => true
  }
}
case class Add(x: Expression, y: Expression) extends Expression {
  override def reduce(env: Map[String, Expression]): Expression = this match {
    case Add(left: Expression, right: Expression) if left.isReducible =>
      Add(left.reduce(env), right)
    case Add(left: Expression, right: Expression) if right.isReducible =>
      Add(left, right.reduce(env))
    case _: Expression =>
      Number(x.value(env) + y.value(env))
  }
  override def toString: String = s"(${x} + ${y})"
}
case class Number(x: Int) extends Expression {
  override def isReducible: Boolean = false
  override def value(env: Map[String, Expression]): Int = x
  override def toString: String = x.toString
}
case class Multiply(x: Expression, y: Expression) extends Expression {
  override def reduce(env: Map[String, Expression]): Expression = this match {
    case Multiply(left: Expression, right: Expression) if left.isReducible =>
      Multiply(left.reduce(env), right)
    case Multiply(left: Expression, right: Expression) if right.isReducible =>
      Multiply(left, right.reduce(env))
    case _: Expression =>
      Number(x.value(env) * y.value(env))
  }
  override def toString: String = s"${x} * ${y}"
}
case class Bool(v: Boolean) extends Expression {
  override def isReducible: Boolean = false
  override def toString: String = if (v) "true" else "false"
}
case class LessThan(left: Expression, right: Expression) extends Expression {
  override def toString: String = s"${left} < ${right}"
  override def reduce(env: Map[String, Expression]): Expression = this match {
    case LessThan(left: Expression, rigth: Expression) if left.isReducible => LessThan(left.reduce(env), rigth)
    case LessThan(left: Expression, rigth: Expression) if right.isReducible => LessThan(left, rigth.reduce(env))
    case LessThan(left: Expression, rigth: Expression) => Bool(left.value(env) < rigth.value(env))
  }
}
case class Variable(name: String) extends Expression {
  override def toString = name
  override def reduce(env: Map[String, Expression]) = env.get(name).orNull
}