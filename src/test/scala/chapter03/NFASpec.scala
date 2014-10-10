package uc.nfa

import org.scalatest._

class NFASpec extends FlatSpec with Matchers {

  "NFARulebook#nextStates" should "return set of state" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, 'A', 2),
      FARule(2, 'B', 3),
      FARule(3, 'C', 1)
    ))
    val nextStates: Set[Int] = rulebook.nextStates(Set(1, 2), 'B')
    nextStates should be (Set(3))
    val nextStates2: Set[Int] = rulebook.nextStates(Set(1), 'C')
    nextStates2 should be (Set())
  }

  "NFA#isAccepting" should "return acception or not" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, 'A', 2),
      FARule(2, 'B', 3),
      FARule(3, 'C', 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.isAccepting should be (true)
    val nfa2: NFA[Int] = NFA(Set(1, 3), Set(2), rulebook)
    nfa2.isAccepting should be (false)
  }

}