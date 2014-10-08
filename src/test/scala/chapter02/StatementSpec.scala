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
    results.last._1 should be (DoNothing())
    results.last._2.toString should be ("Map(x -> 3, y -> 4)")
  }
  "If" should "have condition and diverge" in {

    val cleanEnv = Map[String, Expression]()

    val condition: Expression = LessThan(Number(3), Number(5))
    val consequence: Statement = Assign(Variable("t"), Number(1))
    val alternative: Statement = Assign(Variable("f"), Number(0))
    val ifStatement: Statement = If(condition, consequence, alternative)
    val machine: Machine = Machine(ifStatement, cleanEnv)
    val results: List[(Statement, Map[String, Expression])] = machine.run
    results.last._2.toString should be ("Map(t -> 1)")

    val condition2 = LessThan(Number(5), Number(3))
    val ifStatement2 = If(condition2, consequence, alternative)
    val machine2 = Machine(ifStatement2, cleanEnv)
    val results2 = machine2.run
    results2.last._2.toString should be ("Map(f -> 0)")
  }

}