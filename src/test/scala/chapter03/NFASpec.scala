package uc.nfa

import org.scalatest._

class NFASpec extends FlatSpec with Matchers {

  "NFARulebook#nextStates" should "return set of state" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('A'), 2),
      FARule(2, Some('B'), 3),
      FARule(3, Some('C'), 1)
    ))
    val nextStates: Set[Int] = rulebook.nextStates(Set(1, 2), Some('B'))
    nextStates should be (Set(3))
    val nextStates2: Set[Int] = rulebook.nextStates(Set(1), Some('C'))
    nextStates2 should be (Set())
  }

  "NFA#isAccepting" should "return acception or not" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('A'), 2),
      FARule(2, Some('B'), 3),
      FARule(3, Some('C'), 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.isAccepting should be (true)
    val nfa2: NFA[Int] = NFA(Set(1, 3), Set(2), rulebook)
    nfa2.isAccepting should be (false)
  }

  "NFA#readCharacter" should "return NFA" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('A'), 2),
      FARule(2, Some('B'), 3),
      FARule(3, Some('C'), 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.readCharacter(Some('B')) should be (NFA(Set(3), Set(2), rulebook))
  }

  "NFA#readString" should "return NFA" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('A'), 2),
      FARule(1, Some('B'), 1),
      FARule(2, Some('B'), 3),
      FARule(3, Some('C'), 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.readString("A") should be (NFA(Set(2), Set(2), rulebook))
    val nfa2: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.readString("BB") should be (NFA(Set(1), Set(2), rulebook))
  }

  "NFA#acceptString" should "return if the string is acceptable" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, Some('A'), 2),
      FARule(1, Some('B'), 1),
      FARule(2, Some('B'), 3),
      FARule(3, Some('C'), 1)
    ))
    val nfa: NFA[Int] = NFA(Set(1, 2), Set(2), rulebook)
    nfa.acceptString("BBA") should be (true)
    nfa.acceptString("CBC") should be (false)
  }

  "NFARulebook#followFreeMoves" should "move to free states" in {
    val rulebook: NFARulebook[Int] = NFARulebook(Set(
      FARule(1, None, 2),
      FARule(1, None, 4),
      FARule(2, Some('A'), 3),
      FARule(3, Some('A'), 2),
      FARule(4, Some('A'), 5),
      FARule(5, Some('A'), 6),
      FARule(6, Some('A'), 4)
    ))
    rulebook.followFreeMoves(Set(1)) should be (Set(1, 2, 4))
  }

}