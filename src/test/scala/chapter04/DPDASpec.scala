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
    rulebook.ruleFor(configuration, Some('(')) should be (PDARule(1, Some('('), 2,      None, List(Some('b'), None)))
  }

  "DPDARulebook#nextConfiguration" should "return configuration rules follow" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(1, Some('('), 2,      None, List(Some('b'), None)      ),
      PDARule(2, Some('('), 2, Some('b'), List(Some('b'), Some('b')) ),
      PDARule(2, Some(')'), 2, Some('b'), List()                     ),
      PDARule(2,      None, 1,      None, List(None)                 )
    ))
    val configuration: PDAConfiguration[Int] = PDAConfiguration(1, List(None))
    val conf1: PDAConfiguration[Int] = rulebook.nextConfiguration(configuration, Some('('))
    conf1 should be (PDAConfiguration(2, List(Some('b'), None)))
    val conf2: PDAConfiguration[Int] = rulebook.nextConfiguration(conf1, Some('('))
    conf2 should be (PDAConfiguration(2, List(Some('b'), Some('b'), None)))
    val conf3: PDAConfiguration[Int] = rulebook.nextConfiguration(conf2, Some(')'))
    conf3 should be (PDAConfiguration(2, List(Some('b'), None)))
  }

  "DPDA#isAccepting" should "return if current configuration is accepted or not" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(1, Some('('), 2,      None, List(Some('b'), None)      ),
      PDARule(2, Some('('), 2, Some('b'), List(Some('b'), Some('b')) ),
      PDARule(2, Some(')'), 2, Some('b'), List()                     ),
      PDARule(2,      None, 1,      None, List(None)                 )
    ))
    val currentConfiguration: PDAConfiguration[Int] = PDAConfiguration(1, List(None))
    val dpda: DPDA[Int] = DPDA(currentConfiguration, Set(1), rulebook)
    dpda.isAccepting should be (true)
  }

  "DPDA#readCharacter" should "return DPDA with next configuration" in {
    val rulebook: DPDARulebook[Int] = DPDARulebook(Set(
      PDARule(1, Some('('), 2,      None, List(Some('b'), None)      ),
      PDARule(2, Some('('), 2, Some('b'), List(Some('b'), Some('b')) ),
      PDARule(2, Some(')'), 2, Some('b'), List()                     ),
      PDARule(2,      None, 1,      None, List(None)                 )
    ))
    val currentConfiguration: PDAConfiguration[Int] = PDAConfiguration(1, List(None))
    val dpda: DPDA[Int] = DPDA(currentConfiguration, Set(1), rulebook)
    dpda.readCharacter(Some('(')) should be (
      DPDA(PDAConfiguration(2, List(Some('b'), None)), Set(1), rulebook))
    dpda.readCharacter(Some('(')).readCharacter(Some('(')) should be (
      DPDA(PDAConfiguration(2, List(Some('b'), Some('b'), None)), Set(1), rulebook))
    dpda.readCharacter(Some('(')).readCharacter(Some('(')).readCharacter(Some(')')) should be (
      DPDA(PDAConfiguration(2, List(Some('b'), None)), Set(1), rulebook))
    dpda.readCharacter(Some('(')).readCharacter(Some('(')).readCharacter(Some(')')).readCharacter(Some(')')) should be (
      DPDA(PDAConfiguration(2, List(None)), Set(1), rulebook))
  }

}