package uc.tm

import org.scalatest._

class TapeSpec extends FlatSpec with Matchers {

  "A Tape" should "have four attributes" in {
    val tape: Tape[Char] = Tape(List('1', '0', '1'), '1', List[Char](), '_')
    tape.toString should be ("#<tape 101(1)>")
    tape.left should be (List('1', '0', '1'))
    tape.middle should be ('1')
    tape.right should be(List[Char]())
    tape.blank should be('_')
  }

}