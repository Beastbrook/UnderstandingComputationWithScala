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
    PDARule(Some(1), None, Some(2), Some('b'), List(Some('b'))),
    PDARule(Some(2), Some('a'), Some(2), Some('a'), List()),
    PDARule(Some(2), Some('b'), Some(2), Some('b'), List()),
    PDARule(Some(2), None, Some(3), None, List(None))
  ))
  val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
  val configurations: Set[PDAConfiguration[Int]] = Set(
    configuration
  )

  "rulesFor" should "return a set of rules whitch is appliable" in {
    rulebook.rulesFor(configuration, Some('a')) should be (Set(
      PDARule(Some(1), Some('a'), Some(1), None, List(Some('a'), None))
    ))
  }

  "followRulesFor" should "return a set of configuration that rule follows" in {
    rulebook.followRulesFor(configuration, Some('a')) should be (Set(
      PDAConfiguration(Some(1), List(Some('a'), None))
    ))
  }

  "nextConfigurations" should "return a set of configuration" in {
    rulebook.nextConfigurations(configurations, Some('a')) should be (Set(
      PDAConfiguration(Some(1), List(Some('a'), None))
    ))
  }

  "followFreeMoves" should "return a set of configurations" in {
    rulebook.followFreeMoves(Set(
      PDAConfiguration(Some(1), List(Some('a'), None)),
      PDAConfiguration(Some(1), List(Some('b'), None))
    )) should be (Set(
      PDAConfiguration(Some(1), List(Some('a'), None)),
      PDAConfiguration(Some(1), List(Some('b'), None)),
      PDAConfiguration(Some(2), List(Some('a'), None)),
      PDAConfiguration(Some(2), List(Some('b'), None))
    ))
  }

}