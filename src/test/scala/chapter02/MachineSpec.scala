package uc.simple

import org.scalatest._

class MachineSpec extends FlatSpec with Matchers {

  "A Machine" should "have Statement and Environment" in {
    val cleanEnv: Map[String, Expression] = Map[String, Expression]()
    val expression: Expression = Add(
      Multiply(Number(2), Number(3)),
      Multiply(Number(3), Number(4))
    )
    val machine: Machine = Machine(Exp(expression), cleanEnv)
    machine.toString should be ("Machine('(2 * 3 + 3 * 4)', 'Map()')")
  }

  "Machine#run" should "return each reduced results list" in {
    val expression = Add(
      Multiply(Number(2), Number(3)),
      Multiply(Number(4), Number(5))
    )
    val machine = Machine(Exp(expression), Map[String, Expression]())
    val results = machine.run

    results.length should be (4)
    results.head._1.toString should be ("(2 * 3 + 4 * 5)")
    results.last._1.toString should be ("26")
  }

}