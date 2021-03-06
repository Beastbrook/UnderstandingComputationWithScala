package uc.simple

import org.scalatest._

class ExpressionSpec extends FlatSpec with Matchers {

  "A Add" should "be created with two Numbers" in {
    val add: Expression = Add(Number(1), Number(2))
    add.toString should be ("(1 + 2)")
  }

  "A Add" should "create (1*2)+(3*4) with two Multiplies" in {
    val exp: Expression = Add(
      Multiply(Number(1), Number(2)),
      Multiply(Number(3), Number(4))
    )
    exp.toString should be ("(1 * 2 + 3 * 4)")
  }

  "Expression::isReducible" should "check if expression is reducible" in {
    val reducible = Add(Number(1), Number(2))
    val unreducible = Number(100)
    Expression.isReducible(reducible) should be (true)
    Expression.isReducible(unreducible) should be (false)
  }

  "Expression#isReducible" should "check if 'this' is reducible" in {
    val add: Expression = Add(Number(1), Number(2))
    val num: Expression = Number(3)
    val mul: Expression = Multiply(Add(Number(1), Number(2)), Number(3))

    add.isReducible should be (true)
    num.isReducible should be (false)
    mul.isReducible should be (true)
  }

  "Expression#reduce" should "reduce expression to most reduced" in {
    var exp: Expression = Add(
      Multiply(Number(2), Number(3)),
      Multiply(Number(5), Number(6))
    )
    exp.reduce(null).reduce(null).reduce(null).toString should be ("36")
  }

  "Boolean" should "have 2 patterns" in {
    val t: Expression = Bool(true).reduce(null)
    val f: Expression = Bool(false).reduce(null)
    t should be (TRUE())
    f should be (FALSE())
  }

  "LessThan" should "be left first when reducing" in {
    val lt: Expression = LessThan(
      Add(Number(3), Number(4)),
      Multiply(Number(4), Number(6))
    )
    lt.reduce(null).toString should be ("7 < 4 * 6")
  }

  "LessThan" should "retrun true when (3 + 4) < 4 * 6" in {
   val lt: Expression = LessThan(
      Add(Number(3), Number(4)),
      Multiply(Number(4), Number(6))
    )
   val result = lt.reduced(null)
   result.toString should be ("true")
  }
  "Variable" should "save Expression" in {
    val cleanEnvironment: Map[String, Expression] = Map[String, Expression]()
    val variableName: String = "hello"
    val variableValue: Expression = Add(Multiply(Number(1), Number(2)), Number(4))
    val env: Map[String, Expression] = cleanEnvironment + (variableName -> variableValue)
    val variable: Expression = Variable(variableName)
    variable.isReducible.toString should be ("true")
    variable.reduce(env).toString should be ("(1 * 2 + 4)")
  }

}