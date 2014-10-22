package uc.pda

case class NPDARulebook[A](rules: Set[PDARule[A]]) {
  def rulesFor(configuration: PDAConfiguration[A], character: Option[Char]): Set[PDARule[A]] =
    rules.filter( _.appliesTo(configuration, character) )
  def followRulesFor(configuration: PDAConfiguration[A], character: Option[Char]): Set[PDAConfiguration[A]] =
    rulesFor(configuration, character).map( _.follow(configuration) )
}