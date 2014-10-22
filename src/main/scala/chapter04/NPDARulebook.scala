package uc.pda

case class NPDARulebook[A](rules: Set[PDARule[A]]) {
  def rulesFor(configuration: PDAConfiguration[A], character: Option[Char]): Set[PDARule[A]] =
    rules.filter( _.appliesTo(configuration, character) )
  def followRulesFor(configuration: PDAConfiguration[A], character: Option[Char]): Set[PDAConfiguration[A]] =
    rulesFor(configuration, character).map( _.follow(configuration) )
  def nextConfigurations(configurations: Set[PDAConfiguration[A]], character: Option[Char]) =
    configurations.flatMap( followRulesFor(_, character) )
  def followFreeMoves(configurations: Set[PDAConfiguration[A]]): Set[PDAConfiguration[A]] = {
    val moreConfigurations: Set[PDAConfiguration[A]] = nextConfigurations(configurations, None)
    if (moreConfigurations.subsetOf(configurations)) configurations
    else followFreeMoves(configurations | moreConfigurations)
  }
}