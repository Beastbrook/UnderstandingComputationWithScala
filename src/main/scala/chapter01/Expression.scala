package uc.expression

abstract class Expression()
case class Add(x: Expression, y: Expression) extends Expression
case class Number(x: Int) extends Expression
case class Multiply(x: Expression, y: Expression) extends Expression