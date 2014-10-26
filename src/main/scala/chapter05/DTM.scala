package uc.tm

case class DTM[A,B](
  currentConfiguration: TMConfiguration[A,B],
  acceptStates: Set[A],
  rulebook: DTMRulebook[A,B]) {
  def isAccepting: Boolean =
    acceptStates.contains(currentConfiguration.state)
  def step: DTM[A,B] =
    DTM(rulebook.nextConfiguration(currentConfiguration), acceptStates, rulebook)
  @scala.annotation.tailrec
  final def run: DTM[A,B] =
    if (isAccepting) this
    else step.run
}