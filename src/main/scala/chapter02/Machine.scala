package uc.simple

case class Machine(expression: Expression) {

  def step: Machine = Machine(expression.reduce)

  override def toString: String = s"Machine('${expression.toString}')"

  def run: List[Expression] = {
    def go(exp: Expression): List[Expression] = exp match {
      case e: Expression if !(e.isReducible) => e :: Nil
      case e: Expression => e :: go(e.reduce)
    }
    go(expression)
  }

}