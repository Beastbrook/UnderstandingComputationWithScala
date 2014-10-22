package uc.pda

case class NPDA[A](
  currentConfigurations: Set[PDAConfiguration[A]],
  acceptStates: Set[Option[A]],
  rulebook: NPDARulebook[A]) {

}