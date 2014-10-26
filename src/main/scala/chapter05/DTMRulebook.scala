package uc.tm

case class DTMRulebook[A,B](rules: Set[TMRule[A,B]]) {
  def ruleFor(configuration: TMConfiguration[A,B]): TMRule[A,B] =
    rules.find( _.appliesTo(configuration) ).getOrElse(null)
  def nextConfiguration(configuration: TMConfiguration[A,B]): TMConfiguration[A,B] =
    ruleFor(configuration).follow(configuration)
  def appliesTo(configuration: TMConfiguration[A,B]): Boolean =
    ruleFor(configuration) != null
}