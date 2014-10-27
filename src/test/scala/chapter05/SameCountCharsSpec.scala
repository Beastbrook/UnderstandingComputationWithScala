package uc.tm

import org.scalatest._

class SameCountCharsSpec extends FlatSpec with Matchers {

  val rulebook: DTMRulebook[Int,Char] = DTMRulebook(Set(
    // search 'a' and scan right
    TMRule(1, Some('X'), 1, Some('X'), RIGHT), // skip 'X'
    TMRule(1, Some('a'), 2, Some('X'), RIGHT), // delete 'a' and state 2
    TMRule(1, Some('_'), 6, Some('_'), LEFT),  // find blank and state accepted

    // search 'b' and scan right
    TMRule(2, Some('a'), 2, Some('a'), RIGHT), // skip 'a'
    TMRule(2, Some('b'), 3, Some('X'), RIGHT), // delete 'b' and state 3
    TMRule(2, Some('X'), 2, Some('X'), RIGHT), // skip 'X'

    // search 'c' and scan right
    TMRule(3, Some('c'), 4, Some('X'), RIGHT), // delte 'c' and state 4
    TMRule(3, Some('X'), 3, Some('X'), RIGHT), // skip 'X'
    TMRule(3, Some('b'), 3, Some('b'), RIGHT), // skip 'b'

    // search tail character and scan right
    TMRule(4, Some('c'), 4, Some('c'), RIGHT), // skip 'c'
    TMRule(4, Some('_'), 5, Some('_'), LEFT),  // find blank and state 5

    // search head character and scan left
    TMRule(5, Some('a'), 5, Some('a'), LEFT),
    TMRule(5, Some('b'), 5, Some('b'), LEFT),
    TMRule(5, Some('c'), 5, Some('c'), LEFT),
    TMRule(5, Some('X'), 5, Some('X'), LEFT),
    TMRule(5, Some('_'), 1, Some('_'), RIGHT)
  ))
  "The rule" should "make machine that accepts [aabbcc]" in {
    val startTape: Tape[Char] = Tape(List[Char](), 'a', List('a', 'b', 'b', 'c', 'c'), '_')
    val dtm: DTM[Int,Char] = DTM(TMConfiguration(1, startTape), Set(6), rulebook)
    dtm.run.isAccepting should be (true)
  }
}