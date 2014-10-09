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
