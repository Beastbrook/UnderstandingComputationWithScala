package uc.simple

case class Machine(statement: Statement, env: Map[String, Expression]) {

  def step: Machine = {
    val next: (Statement, Map[String, Expression]) = statement.reduce(env)
    Machine(next._1, next._2)
  }

  override def toString: String = s"Machine('${statement.toString}', '${env}')"

  def run: List[(Statement, Map[String, Expression])] = {
    def go(state: Statement, e: Map[String, Expression]): List[(Statement, Map[String, Expression])] = state match {
      case s: Statement if !(s.isReducible) => (s, e) :: Nil
      case s: Statement => {
        val next: (Statement, Map[String, Expression]) = s.reduce(env)
        (s, e) :: go(next._1, next._2)
      }
    }
    go(statement, env)
  }

}