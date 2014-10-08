package uc.simple

case class Machine(statement: Statement, env: Map[String, Expression]) {

  def step: Machine = {
    val next: (Statement, Map[String, Expression]) = statement.reduce(env)
    Machine(next._1, next._2)
  }

  override def toString: String = s"Machine('${statement.toString}', '${env}')"

  def run: List[(Statement, Map[String, Expression])] = {
    def go(machine: Machine): List[(Statement, Map[String, Expression])] = machine match {
      case Machine(s, e) if !(s.isReducible) => (s, e) :: Nil
      case _ => {
        val nextMachine: Machine = machine.step
        (machine.statement, machine.env) :: go(Machine(nextMachine.statement, nextMachine.env))
      }
    }
    go(this)
  }

}