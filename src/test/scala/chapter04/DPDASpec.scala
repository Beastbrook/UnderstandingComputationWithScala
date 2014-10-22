package uc.pda

import org.scalatest._

class DPDASpec extends FlatSpec with Matchers {

  "isAccepting" should "return if current configuration is accepted or not" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val currentConfiguration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(currentConfiguration, Set(Some(1)), rulebook)
    dpda.isAccepting should be (true)
  }

  "readCharacter" should "return DPDA with next configuration" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val currentConfiguration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(currentConfiguration, Set(Some(1)), rulebook)
    dpda.readCharacter(Some('(')) should be (
      DPDA(PDAConfiguration(Some(2), List(Some('b'), None)), Set[Option[Int]](Some(1)), rulebook)
    )
    dpda.readCharacter(Some('(')).readCharacter(Some('(')) should be (
      DPDA(PDAConfiguration(Some(2), List(Some('b'), Some('b'), None)), Set[Option[Int]](Some(1)), rulebook)
    )
    dpda.readCharacter(Some('(')).readCharacter(Some('(')).readCharacter(Some(')')) should be (
      DPDA(PDAConfiguration(Some(2), List(Some('b'), None)), Set[Option[Int]](Some(1)), rulebook)
    )
    dpda.readCharacter(Some('(')).readCharacter(Some('(')).readCharacter(Some(')')).readCharacter(Some(')')) should be (
      DPDA(PDAConfiguration(Some(1), List(None)), Set[Option[Int]](Some(1)), rulebook)
    )
  }

  "readString" should "return DPDA with next configuration" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val currentConfiguration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(currentConfiguration, Set[Option[Int]](Some(1)), rulebook)
    dpda.readString("(())") should be (DPDA(PDAConfiguration(Some(1), List(None)), Set[Option[Int]](Some(1)), rulebook))
  }

  "readString" should "accept '(())' and not accept '(()('" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(configuration, Set[Option[Int]](Some(1)), rulebook)

    dpda.readString("(()(").isAccepting should be (false)
    dpda.readString("(())").isAccepting should be (true)
  }

  "accepts" should "return if arg is acceptable or not" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('('), Some(2),      None, List(Some('b'), None)      ),
      PDARule(Some(2), Some('('), Some(2), Some('b'), List(Some('b'), Some('b')) ),
      PDARule(Some(2), Some(')'), Some(2), Some('b'), List()                     ),
      PDARule(Some(2),      None, Some(1),      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(configuration, Set[Option[Int]](Some(1)), rulebook)

    dpda.accepts("((((())())))") should be (true)
    dpda.accepts("(()()()()(()))") should be (true)
    dpda.accepts("(()(()())") should be (false)
  }

  it should "be able to treat with two kinds of parameters" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('a'), Some(2), None, List(Some('a'), None)),
      PDARule(Some(1), Some('b'), Some(2), None, List(Some('b'), None)),
      PDARule(Some(2), Some('a'), Some(2), Some('a'), List(Some('a'), Some('a'))),
      PDARule(Some(2), Some('b'), Some(2), Some('b'), List(Some('b'), Some('b'))),
      PDARule(Some(2), Some('a'), Some(2), Some('b'), List()),
      PDARule(Some(2), Some('b'), Some(2), Some('a'), List()),
      PDARule(Some(2), None, Some(1), None, List(None))
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(configuration, Set(Some(1)), rulebook)
    dpda.accepts("ab") should be (true)
    dpda.accepts("aba") should be (false)
    dpda.accepts("ababab") should be (true)
  }

  it should "be able to accept palindrome only if middle is specialized" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(Some(1), Some('a'), Some(1), None, List(Some('a'), None)),
      PDARule(Some(1), Some('a'), Some(1), Some('a'), List(Some('a'), Some('a'))),
      PDARule(Some(1), Some('a'), Some(1), Some('b'), List(Some('a'), Some('b'))),
      PDARule(Some(1), Some('b'), Some(1), None, List(Some('b'), None)),
      PDARule(Some(1), Some('b'), Some(1), Some('b'), List(Some('b'), Some('b'))),
      PDARule(Some(1), Some('b'), Some(1), Some('a'), List(Some('b'), Some('a'))),
      PDARule(Some(1), Some('m'), Some(2), None, List(None)),
      PDARule(Some(1), Some('m'), Some(2), Some('a'), List(Some('a'))),
      PDARule(Some(1), Some('m'), Some(2), Some('b'), List(Some('b'))),
      PDARule(Some(2), Some('a'), Some(2), Some('a'), List()),
      PDARule(Some(2), Some('b'), Some(2), Some('b'), List()),
      PDARule(Some(2), None, Some(3), None, List(None))
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
    val dpda: DPDA[Int] = DPDA(configuration, Set(Some(3)), rulebook)

    dpda.accepts("abmba") should be (true)
    dpda.accepts("ababmabab") should be (false)
    dpda.accepts("babababbmbbababab") should be (true)
  }

}