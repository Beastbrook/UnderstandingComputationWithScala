package uc.regularexpression

sealed trait Pattern {
  def precidence: Int
  def bracket(outerPrecedence: Int) =
    if (precidence < outerPrecedence) "(" + toString + ")"
    else toString
  def inspect: String = s"/${toString}/"
}
case class Empty() extends Pattern {
  override def toString: String = ""
  override def precidence: Int = 3
}
case class Literal(character: Char) extends Pattern {
  override def toString: String = character.toString
  override def precidence: Int = 3
}
case class Concatenate(first: Pattern, second: Pattern) extends Pattern {
  override def toString: String =
    first.bracket(precidence) + second.bracket(precidence)
  override def precidence: Int = 1
}
case class Choose(first: Pattern, second: Pattern) extends Pattern {
  override def toString: String =
    first.bracket(precidence) + "|" + second.bracket(precidence)
  override def precidence: Int = 0
}
case class Repeat(pattern: Pattern) extends Pattern {
  override def toString: String = pattern.bracket(precidence) + "*"
  override def precidence: Int = 2
}