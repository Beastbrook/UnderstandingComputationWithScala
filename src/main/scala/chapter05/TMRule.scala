package uc.tm

case class TMConfiguration[A,B](state: A, tape: Tape[B])

case class TMRule[A,B](
  state: A,
  character: Option[B],
  nextState: A,
  writeCharacter: Option[B],
  direction: String) {

  def appliesTo(configuration: TMConfiguration[A,B]) =
    state == configuration.state && character == Some(configuration.tape.middle)
}