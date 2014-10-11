package uc.nfasimulation

import org.scalatest._
import uc.nfa._
import uc.dfa._
import uc.rule._

class NFASimulationSpec extends FlatSpec with Matchers {

  "NFASimulation" should "build DFA from NFA" in {
    val nfaRulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('a'), 1), FARule(1, Some('a'), 2), FARule(1, None, 2),
      FARule(2, Some('b'), 3),
      FARule(3, None, 2), FARule(3, Some('b'), 1)
    ))
    val nfa: NFA[Int] = NFA(Set(2, 3), Set(3), nfaRulebook)
    val simulation: NFASimulation[Int] = NFASimulation(nfa)
    simulation.nextState(Set(1, 2), Some('a')) should be (Set(1, 2))
    simulation.nextState(Set(1, 2), Some('b')) should be (Set(2, 3))
    simulation.nextState(Set(2, 3), Some('b')) should be (Set(1, 2, 3))
  }

  "NFASimulation#rulesFor" should "make state table" in {
    val nfaRulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('a'), 1), FARule(1, Some('a'), 2), FARule(1, None, 2),
      FARule(2, Some('b'), 3),
      FARule(3, None, 2), FARule(3, Some('b'), 1)
    ))
    val nfa: NFA[Int] = NFA(Set(2, 3), Set(3), nfaRulebook)
    val simulation: NFASimulation[Int] = NFASimulation(nfa)
    simulation.rulesFor(Set(1, 2)) should be (Set(
      FARule(Set(1, 2), Some('a'), Set(1, 2)),
      FARule(Set(1, 2), Some('b'), Set(2, 3))
    ))
    simulation.rulesFor(Set(2, 3)) should be (Set(
      FARule(Set(2, 3), Some('a'), Set()),
      FARule(Set(2, 3), Some('b'), Set(1, 2, 3))
    ))
  }

}