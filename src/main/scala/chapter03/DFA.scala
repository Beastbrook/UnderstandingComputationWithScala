package uc.dfa

sealed trait Rule[A] {
  def canApplyTo(s: A, ch: Char): Boolean
  def follow: Option[A]
}
case class FARule[A](state: A, character: Char, nextState: A) extends Rule[A] {
  override def canApplyTo(s: A, ch: Char): Boolean = state == s && character == ch
  override def follow: Option[A] = Some(nextState)
  override def toString: String = s"FARule<${state} -- ${character} --> ${nextState}>"
}
case class NoRule[A]() extends Rule[A] {
  override def toString: String = "No Rule"
  override def follow: Option[A] = None
  override def canApplyTo(s: A, ch: Char): Boolean = false
}

sealed trait Rulebook[A] {
  def nextState(state: A, character: Char): Option[A]
  def ruleFor(state: A, character: Char): Rule[A]
}
case class DFARulebook[A](rules: List[Rule[A]]) extends Rulebook[A] {
  override def nextState(state: A, character: Char): Option[A] = ruleFor(state, character).follow
  override def ruleFor(state: A, character: Char): Rule[A] = rules.find( _.canApplyTo(state, character) ).getOrElse(NoRule())
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