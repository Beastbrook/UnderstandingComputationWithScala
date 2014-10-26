package uc.tm

case class TMConfiguration[A,B](state: A, tape: Tape[B])

sealed trait Direction
case object LEFT extends Direction
case object RIGHT extends Direction

case class TMRule[A,B](
  state: A,
  character: Option[B],
  nextState: A,
  writeCharacter: Option[B],
  direction: Direction) {

  def appliesTo(configuration: TMConfiguration[A,B]) =
    state == configuration.state && character == Some(configuration.tape.middle)
  def nextTape(configuration: TMConfiguration[A,B]): Tape[B] = direction match {
    case LEFT => configuration.tape.write(writeCharacter.get).moveHeadLeft
    case RIGHT => configuration.tape.write(writeCharacter.get).moveHeadRight
  }
  def follow(configuration: TMConfiguration[A,B]): TMConfiguration[A,B] =
    TMConfiguration(nextState, nextTape(configuration))
}