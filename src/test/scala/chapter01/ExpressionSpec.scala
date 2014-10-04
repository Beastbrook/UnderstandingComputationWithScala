package uc.expression

import org.scalatest._

class ExpressionSpec extends FlatSpec with Matchers {

  "A Add" should "be created with two Numbers" in {
    val add: Expression = Add(Number(1), Number(2))
    add.toString should be ("Add(Number(1),Number(2))")
  }

  "A Add" should "create (1*2)+(3*4) with two Multiplies" in {
    val exp: Expression = Add(
      Multiply(Number(1), Number(2)),
      Multiply(Number(3), Number(4))
    )
    exp.toString should be ("Add(Multiply(Number(1),Number(2)),Multiply(Number(3),Number(4)))")
  }

}