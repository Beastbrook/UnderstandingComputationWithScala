package uc.nfa

import uc.rule._

case class NFARulebook[A](rules: Set[Rule[A]]) {
  def nextStates(states: Set[A], character: Option[Char]): Set[A] =
    states.flatMap( followRulesFor(_, character) )
      .filter( _ != None )
      .map ( _.get )
  def followRulesFor(state: A, character: Option[Char]): Set[Option[A]] =
    rulesFor(state, character).map( _.follow )
  def rulesFor(state: A, character: Option[Char]): Set[Rule[A]] =
    rules.filter( _.canApplyTo(state, character) )
  def followFreeMoves(states: Set[A]): Set[A] = {
    val moreStates: Set[A] = nextStates(states, None)
    if (moreStates.subsetOf(states)) states
    else followFreeMoves(states | moreStates)
  }
}

case class NFA[A](currentStates: Set[A], acceptStates: Set[A], rulebook: NFARulebook[A]) {
  def isAccepting: Boolean =
    (reachableStates & acceptStates).size != 0
  def readCharacter(character: Option[Char]): NFA[A] =
    NFA(rulebook.nextStates(reachableStates, character), acceptStates, rulebook)
  def readString(string: String): NFA[A] =
    if (string == "") this
    else readCharacter(Some(string.head)).readString(string.tail)
  def acceptString(string: String): Boolean =
    readString(string).isAccepting
  def reachableStates: Set[A] =
    rulebook.followFreeMoves(currentStates)
}