package uc.lambda.lc


sealed trait LC {
  def replace(name: String, replacement: LC): LC
}
case class LCVariable(name: String) extends LC {
  override def toString: String =
    name.toString
  def replace(name: String, replacement: LC): LC =
    if (this.name == name) replacement
    else this
}
case class LCFunction(parameter: String, body: LC) extends LC {
  override def toString: String =
    s"-> ${parameter} { ${body} }"
  override def replace(name: String, replacement: LC): LC =
    if (parameter == name) this
    else LCFunction(parameter, body.replace(name, replacement))
  def call(arg: LC): LC =
    body.replace(parameter, arg)
}
case class LCCall(left: LC, right: LC) extends LC {
  override def toString: String =
    s"${left}[${right}]"
  def replace(name: String, replacement: LC): LC =
    LCCall(left.replace(name, replacement), right.replace(name, replacement))
}