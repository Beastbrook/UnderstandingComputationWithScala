package uc.pda

case class PDAConfiguration[A](state: Option[A], stack: List[Option[Char]]) {
  val STUCK_STATE: Option[A] = None
  def stuck: PDAConfiguration[A] = PDAConfiguration(STUCK_STATE, stack)
  def isStuck: Boolean = state == STUCK_STATE
}

case class PDARule[A](
  state: Option[A],
  character: Option[Char],
  nextState: Option[A],
  popCharacter: Option[Char],
  pushCharacters: List[Option[Char]]) {
  def appliesTo(configuration: PDAConfiguration[A], ch: Option[Char]): Boolean =
    state == configuration.state &&
      popCharacter == configuration.stack.head &&
      character == ch
  def follow(configuration: PDAConfiguration[A]): PDAConfiguration[A] =
    PDAConfiguration(nextState, nextStack(configuration))
  def nextStack(configuration: PDAConfiguration[A]): List[Option[Char]] =
    pushCharacters ++ configuration.stack.tail
}