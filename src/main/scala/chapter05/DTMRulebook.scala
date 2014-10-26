package uc.tm

case class DTMRulebook[A,B](rules: Set[TMRule[A,B]]) {
  def ruleFor(configuration: TMConfiguration[A,B]): TMRule[A,B] =
    rules.find( _.appliesTo(configuration) ).get
  def nextConfiguration(configuration: TMConfiguration[A,B]): TMConfiguration[A,B] =
    ruleFor(configuration).follow(configuration)
}