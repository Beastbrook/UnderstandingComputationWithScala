package uc.pda

import org.scalatest._

class NPDARulebookSpec extends FlatSpec with Matchers {

  val rulebook: NPDARulebook[Int] = NPDARulebook(Set(
    PDARule(Some(1), Some('a'), Some(1), None, List(Some('a'), None)),
    PDARule(Some(1), Some('a'), Some(1), Some('a'), List(Some('a'), Some('a'))),
    PDARule(Some(1), Some('a'), Some(1), Some('b'), List(Some('a'), Some('b'))),
    PDARule(Some(1), Some('b'), Some(1), None, List(Some('b'), None)),
    PDARule(Some(1), Some('b'), Some(1), Some('b'), List(Some('b'), Some('b'))),
    PDARule(Some(1), Some('b'), Some(1), Some('a'), List(Some('b'), Some('a'))),
    PDARule(Some(1), None, Some(2), None, List(None)),
    PDARule(Some(1), None, Some(2), Some('a'), List(Some('a'))),
    PDARule(Some(1), None, Some(2), Some('b'), List(Some('a'))),
    PDARule(Some(2), Some('a'), Some(2), Some('a'), List()),
    PDARule(Some(2), Some('b'), Some(2), Some('b'), List()),
    PDARule(Some(2), None, Some(3), None, List(Some('a'), Some('b')))
  ))

  "rulesFor" should "return set of rules whitch is appliable" in {
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    rulebook.rulesFor(configuration, Some('a')) should be (Set(
      PDARule(Some(1), Some('a'), Some(1), None, List(Some('a'), None))
    ))
  }

}