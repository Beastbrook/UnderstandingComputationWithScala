package uc.simple

import org.scalatest._

class Statement extends FlatSpec with Matchers {

  "Assign#toString" should "return 'x = 2 + 3'" in {
    val assign: Statement = Assign(Variable("x"), Add(Number(2), Number(3)))
    assign.toString should be ("x = (2 + 3)")
  }

}