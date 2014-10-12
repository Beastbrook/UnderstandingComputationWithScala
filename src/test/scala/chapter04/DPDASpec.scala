package uc.pda

import org.scalatest._

class DPDASpec extends FlatSpec with Matchers {

  "DPDARulebook#ruleFor" should "return appliable states" in {

    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(1, Some('('), 2,      None, List(Some('b'), None)      ),
      PDARule(2, Some('('), 2, Some('b'), List(Some('b'), Some('b')) ),
      PDARule(2, Some(')'), 2, Some('b'), List()                     ),
      PDARule(2,      None, 1,      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(1, List(None))
    rulebook.ruleFor(configuration, Some('(')) should be (Set[PDARule[Int]](
      PDARule(1, Some('('), 2,      None, List(Some('b'), None))
    ))
  }
}