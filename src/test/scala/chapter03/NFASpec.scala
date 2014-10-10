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

  "NFA#readCharacter" should "return NFA" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, 'A', 2),
      FARule(2, 'B', 3),
      FARule(3, 'C', 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.readCharacter('B') should be (NFA(Set(3), Set(2), rulebook))
  }

  "NFA#readString" should "return NFA" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, 'A', 2),
      FARule(1, 'B', 1),
      FARule(2, 'B', 3),
      FARule(3, 'C', 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.readString("A") should be (NFA(Set(2), Set(2), rulebook))
    val nfa2: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.readString("BB") should be (NFA(Set(1), Set(2), rulebook))
  }

  "NFA#acceptString" should "return if the string is acceptable" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, 'A', 2),
      FARule(1, 'B', 1),
      FARule(2, 'B', 3),
      FARule(3, 'C', 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.acceptString("BBA") should be (true)
    nfa.acceptString("CBC") should be (false)
  }

}