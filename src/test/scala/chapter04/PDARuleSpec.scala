package uc.pda

import org.scalatest._

class PDARuleSpec extends FlatSpec with Matchers {

  "PDARule#appliesTo" should "return if state and stack is appliable" in {
    val rule: PDARule[Int] = PDARule(
      1,
      Some('('),
      2,
      None,
      List(Some('b'), None)
    )
    val configuration: PDAConfiguration[Int] = PDAConfiguration(1, List(None))
    rule.appliesTo(configuration, Some('(')) should be (true)
  }

}