package uc.tm

import org.scalatest._

class TMRuleSpec extends FlatSpec with Matchers {

  val rule: TMRule[Int,Char] = TMRule(1, Some('0'), 2, Some('1'), "RIGHT")

  "appliesTo" should "return if appliable to configuration or not" in {
    rule.appliesTo(TMConfiguration(1, Tape(List[Char](), '0', List[Char](), '_'))) should be (true)
    rule.appliesTo(TMConfiguration(1, Tape(List[Char](), '1', List[Char](), '_'))) should be (false)
    rule.appliesTo(TMConfiguration(2, Tape(List[Char](), '0', List[Char](), '_'))) should be (false)
  }

}