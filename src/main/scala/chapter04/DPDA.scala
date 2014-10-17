package uc.pda

case class DPDARulebook[A](rules: Set[PDARule[A]]) {
  def nextConfiguration(configuration: PDAConfiguration[A], character: Option[Char]): PDAConfiguration[A] =
    ruleFor(configuration, character).follow(configuration)
  def ruleFor(configuration: PDAConfiguration[A], character: Option[Char]): PDARule[A] =
    rules.find( _.appliesTo(configuration, character) ).getOrElse(null)
  def appliesTo(configuration: PDAConfiguration[A], character: Option[Char]): Boolean =
    !(ruleFor(configuration, character) == null)
  def followFreeMoves(configuration: PDAConfiguration[A]): PDAConfiguration[A] =
    if (appliesTo(configuration, None)) followFreeMoves(nextConfiguration(configuration, None))
    else configuration
}

case class DPDA[A](
  currentConfiguration: PDAConfiguration[A],
  acceptStates: Set[A],
  rulebook: DPDARulebook[A]) {
  def isAccepting: Boolean = acceptStates.contains(currentConfiguration.state)
  def readCharacter(character: Option[Char]): DPDA[A] =
    DPDA(rulebook.nextConfiguration(currentConfiguration, character), acceptStates, rulebook)
  def readString(string: String): DPDA[A] =
    if (string == "") this
    else readCharacter(Some(string.head)).readString(string.tail)
}