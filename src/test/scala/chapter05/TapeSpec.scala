package uc.tm

import org.scalatest._

class TapeSpec extends FlatSpec with Matchers {

  val tape: Tape[Char] = Tape(List('1', '0', '1'), '1', List[Char](), '_')

  "A Tape" should "have four attributes" in {
    tape.toString should be ("#<tape 101(1)>")
    tape.left should be (List('1', '0', '1'))
    tape.middle should be ('1')
    tape.right should be(List[Char]())
    tape.blank should be('_')
  }

  "moveHeadLeft" should "return new Tape that moved to left" in {
    tape.moveHeadLeft.toString should be ("#<tape 10(1)1>")
  }

  "moveHeadRight" should "return new Tape that moved to right" in {
    tape.moveHeadLeft.moveHeadRight should be (tape)
    tape.moveHeadRight.toString should be ("#<tape 1011(_)>")
  }

  "write" should "return new tape that middle is replaced" in {
    tape.write('0').toString should be ("#<tape 101(0)>")
    tape.moveHeadRight.write('1').toString should be ("#<tape 1011(1)>")
  }

}