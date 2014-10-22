package uc.pda

case class NPDA[A](
  currentConfigurations: Set[PDAConfiguration[A]],
  acceptStates: Set[Option[A]],
  rulebook: NPDARulebook[A]) {
  def current: Set[PDAConfiguration[A]] =
    rulebook.followFreeMoves(currentConfigurations)
  def isAccepting: Boolean =
    current.exists( (c) => acceptStates.contains(c.state) )
}