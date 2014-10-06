package uc.simple

abstract class Statement {
  def isReducible: Boolean = true
  def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = (this, env)
}
case class DoNothing() extends Statement {
  override def toString: String = "DoNothing"
  override def isReducible: Boolean = false
}
case class Assign(v: Variable, expression: Expression) extends Statement {
  override def toString: String = s"${v} = ${expression}"
  override def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = expression match {
    case e: Expression if !(e.isReducible) => (DoNothing(), env + (v.name -> e))
    case e: Expression => (Assign(v, e.reduce(env)), env)
  }
}