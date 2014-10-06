package uc.simple

case class Machine(expression: Expression, env: Map[String, Expression]) {

  def step(env: Map[String, Expression]): Machine = Machine(expression.reduce(env), env)

  override def toString: String = s"Machine('${expression.toString}')"

  def run: List[Expression] = {
    def go(exp: Expression): List[Expression] = exp match {
      case e: Expression if !(e.isReducible) => e :: Nil
      case e: Expression => e :: go(e.reduce(env))
    }
    go(expression)
  }

}