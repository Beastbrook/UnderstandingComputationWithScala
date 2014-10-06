package uc.simple

import org.scalatest._

class MachineSpec extends FlatSpec with Matchers {

  "A Machine" should "have a Expression" in {
    val expression: Expression = Add(
      Multiply(Number(2), Number(3)),
      Multiply(Number(3), Number(4))
    )
    val machine: Machine = Machine(expression, Map[String, Expression]())
    val machineNextOneStep: Machine = machine.step(null)
    val machineNextTwoStep: Machine = machineNextOneStep.step(null)

    machineNextOneStep.toString should be ("Machine('(6 + 3 * 4)')")
    machineNextTwoStep.toString should be ("Machine('(6 + 12)')")
  }

  "Machine#run" should "return each reduced results list" in {
    val expression = Add(
      Multiply(Number(2), Number(3)),
      Multiply(Number(4), Number(5))
    )
    val machine = Machine(expression, Map[String, Expression]())
    val results = machine.run

    results.length should be (4)
    results.head.toString should be ("(2 * 3 + 4 * 5)")
    results.last.toString should be ("26")
  }

}