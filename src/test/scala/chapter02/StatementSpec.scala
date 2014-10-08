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

  "Sequence" should "behave as Statement List" in {
    val seq: Statement = Sequence(List(
      Assign(Variable("x"), Number(3)),
      Assign(Variable("y"), Add(Variable("x"), Number(1))),
      Exp(Variable("y"))
    ))
    val cleanEnv: Map[String, Expression] = Map[String, Expression]()
    val machine: Machine = Machine(seq, cleanEnv)
    val results: List[(Statement, Map[String, Expression])] = machine.run
  }

}