package uc.lambda.lc


sealed trait LC
case class LCVariable(name: String) extends LC {
  override def toString: String =
    name.toString
}
case class LCFunction(parameter: String, body: LC) extends LC {
  override def toString: String =
    s"-> ${parameter} { ${body} }"
}
case class LCCall(left: LC, right: LC) extends LC {
  override def toString: String =
    s"${left}[${right}]"
}