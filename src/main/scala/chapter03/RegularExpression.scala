package uc.regularexpression

import uc.nfa._

sealed trait Pattern {
  def precidence: Int
  def toNFA: NFA[Object]
  def matches(string: String): Boolean = toNFA.acceptString(string)
  def bracket(outerPrecedence: Int) =
    if (precidence < outerPrecedence) "(" + toString + ")"
    else toString
  def inspect: String = s"/${toString}/"
}
case class Empty() extends Pattern {
  override def toString: String = ""
  override def toNFA: NFA[Object] = {
    val startState: Object = new Object()
    val acceptStates: Set[Object] = Set(startState)
    val rulebook: NFARulebook[Object] = NFARulebook(Set[Rule[Object]]())
    NFA(Set(startState), acceptStates, rulebook)
  }
  override def precidence: Int = 3
}
case class Literal(character: Char) extends Pattern {
  override def toString: String = character.toString
  override def toNFA: NFA[Object] = {
    val startState: Object = new Object()
    val acceptState: Object = new Object()
    val acceptStates: Set[Object] = Set(acceptState)
    val rule: FARule[Object] = FARule(startState, Some(character), acceptState)
    val rulebook: NFARulebook[Object] = NFARulebook(Set(rule))
    NFA(Set(startState), acceptStates, rulebook)
  }
  override def precidence: Int = 3
}
case class Concatenate(first: Pattern, second: Pattern) extends Pattern {
  override def toString: String =
    first.bracket(precidence) + second.bracket(precidence)
  override def toNFA: NFA[Object] = {
    val firstNFA: NFA[Object] = first.toNFA
    val secondNFA: NFA[Object] = second.toNFA
    val startStates: Set[Object] = firstNFA.currentStates
    val acceptStates: Set[Object] = secondNFA.acceptStates
    val extraRules: Set[Rule[Object]] =
      firstNFA
        .acceptStates
        .flatMap( (from) => secondNFA.currentStates.map( (to) => FARule[Object](from, None, to) ) )
    val rules: Set[Rule[Object]] = extraRules | firstNFA.rulebook.rules | secondNFA.rulebook.rules
    val rulebook: NFARulebook[Object] = NFARulebook[Object](rules)
    NFA[Object](startStates, acceptStates, rulebook)
  }
  override def precidence: Int = 1
}
case class Choose(first: Pattern, second: Pattern) extends Pattern {
  override def toString: String =
    first.bracket(precidence) + "|" + second.bracket(precidence)
  override def toNFA: NFA[Object] = {
    val firstNFA: NFA[Object] = first.toNFA
    val secondNFA: NFA[Object] = second.toNFA
    val startState: Object = new Object()
    val acceptStates: Set[Object] = firstNFA.acceptStates | secondNFA.acceptStates
    val extraRules: Set[Rule[Object]] =
      (firstNFA.currentStates | secondNFA.currentStates)
        .map( (state) => FARule(startState, None, state) )
    val rules: Set[Rule[Object]] = extraRules | firstNFA.rulebook.rules | secondNFA.rulebook.rules
    val rulebook: NFARulebook[Object] = NFARulebook(rules)
    NFA(Set(startState), acceptStates, rulebook)
  }
  override def precidence: Int = 0
}
case class Repeat(pattern: Pattern) extends Pattern {
  override def toString: String = pattern.bracket(precidence) + "*"
  override def toNFA: NFA[Object] = null
  override def precidence: Int = 2
}