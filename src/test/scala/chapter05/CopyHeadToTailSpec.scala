package uc.tm

import org.scalatest._

class CopyHeadToTailSpec extends FlatSpec with Matchers {

  val rulebook: DTMRulebook[Int,Char] = DTMRulebook(Set(
    // read head character
    TMRule(1, Some('a'), 2, Some('a'), RIGHT),
    TMRule(1, Some('b'), 3, Some('b'), RIGHT),
    TMRule(1, Some('c'), 4, Some('c'), RIGHT),

    // search tail and scan right (keep: a)
    TMRule(2, Some('a'), 2, Some('a'), RIGHT),
    TMRule(2, Some('b'), 2, Some('b'), RIGHT),
    TMRule(2, Some('c'), 2, Some('c'), RIGHT),
    TMRule(2, Some('_'), 5, Some('a'), RIGHT),

    // search tail and scan right (keep: b)
    TMRule(3, Some('a'), 3, Some('a'), RIGHT),
    TMRule(3, Some('b'), 3, Some('b'), RIGHT),
    TMRule(3, Some('c'), 3, Some('c'), RIGHT),
    TMRule(3, Some('_'), 5, Some('b'), RIGHT),

    // search tail and scan right (keep: c)
    TMRule(3, Some('a'), 4, Some('a'), RIGHT),
    TMRule(3, Some('b'), 4, Some('b'), RIGHT),
    TMRule(3, Some('c'), 4, Some('c'), RIGHT),
    TMRule(3, Some('_'), 5, Some('c'), RIGHT)
  ))

  "The rule" should "copy head character to tail" in {
    val startTape: Tape[Char] = Tape(List[Char](), 'a', List('a', 'b', 'b', 'c', 'c'), '_')
    val dtm: DTM[Int,Char] = DTM(TMConfiguration(1, startTape), Set(5), rulebook)
    dtm.run.isAccepting should be (true)
    dtm.run.currentConfiguration.tape.toString should be ("#<tape aabbcca(_)>")
  }

}