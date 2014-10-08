package uc.simple

trait Statement {
  def isReducible: Boolean = true
  def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression])
}
case class DoNothing() extends Statement {
  override def toString: String = "DoNothing"
  override def isReducible: Boolean = false
  override def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = (this, env)
}
case class Assign(v: Variable, expression: Expression) extends Statement {
  override def toString: String = s"${v} = ${expression}"
  override def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = expression match {
    case e: Expression if !(e.isReducible) => (DoNothing(), env + (v.name -> e))
    case e: Expression => (Assign(v, e.reduce(env)), env)
  }
}
case class Exp(expression: Expression) extends Statement {
  override def toString: String = s"${expression}"
  override def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = expression match {
    case e: Expression if !(e.isReducible) => (Exp(e), env)
    case e: Expression => (Exp(e.reduce(env)), env)
  }
  override def isReducible: Boolean = expression.isReducible
}
case class Sequence(statements: List[Statement]) extends Statement {
  override def toString: String = statements.mkString("[\n", "\n", "\n]")
  override def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = statements match {
    case Nil => (DoNothing(), env)
    case x :: Nil if x.isReducible => {
      val next = x.reduce(env)
      (Sequence(next._1 :: Nil), next._2)
    }
    case x :: Nil => (DoNothing(), env)
    case x :: xs if x.isReducible => {
      val next = x.reduce(env)
      (Sequence(next._1 :: xs), next._2)
    }
    case _ :: xs => (Sequence(xs), env)
  }
  override def isReducible: Boolean = if (statements.size == 0) false else true
}
case class If(condition: Expression, consequence: Statement, alternative: Statement) extends Statement {
  override def toString: String = s"if(${condition}) { ${consequence} } else { ${alternative} }"
  override def reduce(env: Map[String, Expression]): (Statement, Map[String, Expression]) = condition.reduced(env) match {
    case TRUE() => (consequence, env)
    case FALSE() => (alternative, env)
    case _ => (null, null)
  }
}