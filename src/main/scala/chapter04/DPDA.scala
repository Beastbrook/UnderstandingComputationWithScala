package uc.pda

case class DPDARulebook[A](rules: Set[PDARule[A]]) {
  def ruleFor(configuration: PDAConfiguration[A], character: Option[Char]) =
    rules.filter( _.appliesTo(configuration, character) )
}