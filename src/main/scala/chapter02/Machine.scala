package uc.expression

case class Machine(expression: Expression) {

  def step: Machine = Machine(expression.reduce)

  override def toString: String = s"Machine('${expression.toString}')"

}