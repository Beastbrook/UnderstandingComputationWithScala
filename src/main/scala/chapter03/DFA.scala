package uc.dfa

import uc.rule._

sealed trait Rulebook[A] {
  def nextState(state: A, character: Char): Option[A]
  def ruleFor(state: A, character: Char): Rule[A]
}
case class DFARulebook[A](rules: List[Rule[A]]) extends Rulebook[A] {
  override def nextState(state: A, character: Char): Option[A] = ruleFor(state, character).follow
  override def ruleFor(state: A, character: Char): Rule[A] = rules.find( _.canApplyTo(state, Some(character)) ).getOrElse(NoRule())
}

case class DFA[A](currentState: A, acceptStates: List[A], rulebook: Rulebook[A]) {
  def isAccepting: Boolean = acceptStates.contains(currentState)
  def readCharacter(ch: Char): DFA[A] = DFA(
    rulebook.nextState(currentState, ch).get,
    acceptStates,
    rulebook
  )
  def readString(string: String): DFA[A] = if (string != "") readCharacter(string.head).readString(string.tail) else this
  def isAcceptable(string: String): Boolean = readString(string).isAccepting
}