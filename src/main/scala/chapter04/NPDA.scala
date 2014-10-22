package uc.pda

case class NPDA[A](
  currentConfigurations: Set[PDAConfiguration[A]],
  acceptStates: Set[Option[A]],
  rulebook: NPDARulebook[A]) {
  def current: Set[PDAConfiguration[A]] =
    rulebook.followFreeMoves(currentConfigurations)
  def isAccepting: Boolean =
    current.exists( (c) => acceptStates.contains(c.state) )
  def readCharacter(character: Option[Char]): NPDA[A] = {
    val next: Set[PDAConfiguration[A]] = rulebook.nextConfigurations(current, character)
    val freeNext: Set[PDAConfiguration[A]] = rulebook.followFreeMoves(next)
    NPDA(freeNext, acceptStates, rulebook)
  }
}