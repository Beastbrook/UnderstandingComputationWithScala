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
}