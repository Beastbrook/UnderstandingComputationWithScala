package uc.tm

import org.scalatest._

class DTMRulebookSpec extends FlatSpec with Matchers {

  // increment binary dicimal
  val rulebook: DTMRulebook[Int,Char] = DTMRulebook(Set(
    TMRule(1, Some('0'), 2, Some('1'), RIGHT),
    TMRule(1, Some('1'), 1, Some('1'), LEFT),
    TMRule(1, Some('_'), 2, Some('1'), RIGHT),
    TMRule(2, Some('0'), 2, Some('1'), RIGHT),
    TMRule(2, Some('1'), 2, Some('1'), RIGHT),
    TMRule(2, Some('_'), 3, Some('1'), LEFT)
  ))
  val start: TMConfiguration[Int,Char] = TMConfiguration(1, Tape(List('1', '0', '1'), '1', List[Char](), '_'))

  "ruleFor" should "search appliable rule" in {
    rulebook.ruleFor(start) should be (TMRule(1, Some('1'), 1, Some('1'), LEFT))
  }
}