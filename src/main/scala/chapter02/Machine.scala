package uc.expression

case class Machine(expression: Expression) {

  def step: Machine = Machine(expression.reduce)

}