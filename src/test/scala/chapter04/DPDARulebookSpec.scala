package uc.pda

import org.scalatest._

class DPDARulebookSpec extends FlatSpec with Matchers {

  "ruleFor" should "return appliable states" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    rulebook.ruleFor(configuration, Some('(')) should be (PDARule(Some(1), Some('('), Some(2), None, List(Some('b'), None)))
  }

  "nextConfiguration" should "return configuration rules follow" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val conf1: PDAConfiguration[Int] = rulebook.nextConfiguration(configuration, Some('('))
    conf1 should be (PDAConfiguration(Some(2), List(Some('b'), None)))
    val conf2: PDAConfiguration[Int] = rulebook.nextConfiguration(conf1, Some('('))
    conf2 should be (PDAConfiguration(Some(2), List(Some('b'), Some('b'), None)))
    val conf3: PDAConfiguration[Int] = rulebook.nextConfiguration(conf2, Some(')'))
    conf3 should be (PDAConfiguration(Some(2), List(Some('b'), None)))
  }

  "appliesTo" should "return if rulebook has the rule or not" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(2), List(None))
    rulebook.appliesTo(configuration, None) should be (true)
  }

  "followFreeMoves" should "return configuration which can be reached freely" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(2), List(None))
    rulebook.followFreeMoves(configuration) should be (PDAConfiguration(Some(1), List(None)))
  }

}