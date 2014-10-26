package uc.tm

case class Tape[A](
  left: List[A],
  middle: A,
  right: List[A],
  blank: A) {
  override def toString: String = {
    val l = left.mkString("", "", "")
    val m = s"(${middle})"
    val r = right.mkString("", "", "")
    s"#<tape ${l}${m}${r}>"
  }
  def moveHeadLeft: Tape[A] =
    if (left.size == 0) Tape(List[A](), blank, middle :: right, blank)
    else Tape(left.reverse.tail.reverse, left.reverse.head, middle :: right, blank)
  def moveHeadRight: Tape[A] =
    if (right.size == 0) Tape((middle :: left.reverse).reverse, blank, List[A](), blank)
    else Tape((middle :: left.reverse).reverse, right.head, right.tail, blank)
  def write(newMiddle: A): Tape[A] =
    Tape(left, newMiddle, right, blank)
}