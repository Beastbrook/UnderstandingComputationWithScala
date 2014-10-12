package uc.pda

case class PDAConfiguration[A](state: A, stack: List[Option[Char]])

case class PDARule[A](
  state: A,
  character: Option[Char],
  nextState: A,
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