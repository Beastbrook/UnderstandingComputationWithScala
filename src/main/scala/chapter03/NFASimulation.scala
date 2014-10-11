package uc.nfasimulation

import uc.nfa._
import uc.dfa._
import uc.rule._

case class NFASimulation[A](nfa: NFA[A]) {
  def nextState(states: Set[A], character: Option[Char]): Set[A] =
    NFA(states, nfa.acceptStates, nfa.rulebook)
      .readCharacter(character)
      .currentStates
}