package uc.pda

import org.scalatest._

class PDARuleSpec extends FlatSpec with Matchers {

  "PDARule#appliesTo" should "return if state and stack is appliable" in {
    val rule: PDARule[Int] = PDARule(
      Some(1),
      Some('('),
      Some(2),
      None,
      List(Some('b'), None)
    )
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    rule.appliesTo(configuration, Some('(')) should be (true)
  }

  "PDARule#follow" should "return configuration of next state" in {
    val rule: PDARule[Int] = PDARule(
      Some(1),
      Some('('),
      Some(2),
      None,
      List(Some('b'), None)
    )
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    rule.follow(configuration) should be (PDAConfiguration(Some(2), List(Some('b'), None)))
  }

}