package uc.rule

sealed trait Rule[A] {
  def canApplyTo(s: A, ch: Option[Char]): Boolean
  def follow: Option[A]
  def ch: Option[Char]
}
case class FARule[A](state: A, character: Option[Char], nextState: A) extends Rule[A] {
  override def canApplyTo(s: A, ch: Option[Char]): Boolean = state == s && character == ch
  override def follow: Option[A] = Some(nextState)
  override def toString: String = s"FARule<${state} -- ${character.getOrElse(' ')} --> ${nextState}>"
  override def ch: Option[Char] = character
}
case class NoRule[A]() extends Rule[A] {
  override def toString: String = "No Rule"
  override def follow: Option[A] = None
  override def canApplyTo(s: A, ch: Option[Char]): Boolean = false
  override def ch: Option[Char] = None
}