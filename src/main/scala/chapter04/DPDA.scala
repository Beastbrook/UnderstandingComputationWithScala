package uc.pda

case class DPDARulebook[A](rules: Set[PDARule[A]]) {
  def nextConfiguration(configuration: PDAConfiguration[A], character: Option[Char]) =
    ruleFor(configuration, character).follow(configuration)
  def ruleFor(configuration: PDAConfiguration[A], character: Option[Char]): PDARule[A] =
    rules.find( _.appliesTo(configuration, character) ).get
}