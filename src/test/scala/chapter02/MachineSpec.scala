package uc.expression

import org.scalatest._

class MachineSpec extends FlatSpec with Matchers {

  "A Machine" should "have a Expression" in {
    val expression: Expression = Add(
      Multiply(Number(2), Number(3)),
      Multiply(Number(3), Number(4))
    )
    val machine: Machine = Machine(expression)
    val machineNextOneStep: Machine = machine.step
    val machineNextTwoStep: Machine = machineNextOneStep.step

    machineNextOneStep.toString should be ("Machine(Add(Number(6),Multiply(Number(3),Number(4))))")
    machineNextTwoStep.toString should be ("Machine(Add(Number(6),Number(12)))")
  }

}