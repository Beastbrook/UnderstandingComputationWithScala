package uc.tm

import org.scalatest._

class SubroutineSpec extends FlatSpec with Matchers {

  val incrementRules: (Int, Int) => Set[TMRule[Object,Char]] = (start: Int, finish: Int) => {
    val incrementing: Option[Int] = Some(start)
    val finishing: Object = new Object()
    val finished: Option[Int] = Some(finish)
    Set(
      TMRule(incrementing, Some('0'), finishing, Some('1'), RIGHT),
      TMRule(incrementing, Some('1'), incrementing, Some('0'), LEFT),
      TMRule(incrementing, Some('_'), finishing, Some('1'), RIGHT),
      TMRule(finishing, Some('0'), finishing, Some('0'), RIGHT),
      TMRule(finishing, Some('1'), finishing, Some('1'), RIGHT),
      TMRule(finishing, Some('_'), finished, Some('_'), LEFT)
    )
  }

  val rulebook: DTMRulebook[Object,Char] = DTMRulebook(
    incrementRules(0, 1) | incrementRules(1, 2) | incrementRules(2, 3)
  )

  "The rule" should "increment three times" in {
    val startTape: Tape[Char] = Tape(List[Char]('1', '0', '1'), '1', List[Char](), '_')
    val dtm: DTM[Object,Char] = DTM(TMConfiguration(Some(0), startTape), Set(Some(3)), rulebook)
    dtm.run.isAccepting should be (true)
    dtm.run.currentConfiguration.tape.toString should be ("#<tape 111(0)_>")
  }
}