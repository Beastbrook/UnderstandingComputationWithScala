package uc.tm

import org.scalatest._

class DTMSpec extends FlatSpec with Matchers {

  // increment binary dicimal
  val rulebook: DTMRulebook[Int,Char] = DTMRulebook(Set(
    TMRule(1, Some('0'), 2, Some('1'), RIGHT),
    TMRule(1, Some('1'), 1, Some('0'), LEFT),
    TMRule(1, Some('_'), 2, Some('1'), RIGHT),
    TMRule(2, Some('0'), 2, Some('0'), RIGHT),
    TMRule(2, Some('1'), 2, Some('1'), RIGHT),
    TMRule(2, Some('_'), 3, Some('_'), LEFT)
  ))
  val start: TMConfiguration[Int,Char] = TMConfiguration(1, Tape(List('1', '0', '1'), '1', List[Char](), '_'))

  "isAccepting" should "return if the state is accepted or not" in {
    DTM(start, Set(1), rulebook).isAccepting should be (true)
    DTM(start, Set(3), rulebook).isAccepting should be (false)
  }

  "step" should "return next DTM" in {
    val dtm: DTM[Int,Char] = DTM(start, Set(3), rulebook)
    dtm.step.currentConfiguration.tape.toString should be ("#<tape 10(1)0>")
    dtm.step.step.currentConfiguration.tape.toString should be ("#<tape 1(0)00>")
    dtm.step.step.step.currentConfiguration.tape.toString should be ("#<tape 11(0)0>")
  }

  "run" should "repeat while state is accepted" in {
    val dtm: DTM[Int,Char] = DTM(start, Set(3), rulebook)
    dtm.run.isAccepting should be (true)
    dtm.run.currentConfiguration.tape.toString should be ("#<tape 110(0)_>")
  }

}