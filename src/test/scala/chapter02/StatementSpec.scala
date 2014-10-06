package uc.simple

import org.scalatest._

class StatementSpec extends FlatSpec with Matchers {

  "Assign#toString" should "return 'x = 2 + 3'" in {
    val assign: Statement = Assign(Variable("x"), Add(Number(2), Number(3)))
    assign.toString should be ("x = (2 + 3)")
  }

  "Assign#reduce" should "return (Statement, Environment))" in {
    val assign: Statement = Assign(Variable("x"), Add(Number(2), Number(3)))
    val cleanEnv: Map[String, Expression] = Map[String, Expression]()
    val onceReduced: Statement = assign.reduce(cleanEnv)._1
    onceReduced.toString should be ("x = 5")
  }

}