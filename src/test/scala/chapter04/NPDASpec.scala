package uc.pda

import org.scalatest._

class NPDASpec extends FlatSpec with Matchers {

  val rulebook: NPDARulebook[Int] = NPDARulebook(Set(
    PDARule(Some(1), Some('a'), Some(1), None, List(Some('a'), None)),
    PDARule(Some(1), Some('a'), Some(1), Some('a'), List(Some('a'), Some('a'))),
    PDARule(Some(1), Some('a'), Some(1), Some('b'), List(Some('a'), Some('b'))),
    PDARule(Some(1), Some('b'), Some(1), None, List(Some('b'), None)),
    PDARule(Some(1), Some('b'), Some(1), Some('b'), List(Some('b'), Some('b'))),
    PDARule(Some(1), Some('b'), Some(1), Some('a'), List(Some('b'), Some('a'))),
    PDARule(Some(1), None, Some(2), None, List(None)),
    PDARule(Some(1), None, Some(2), Some('a'), List(Some('a'))),
    PDARule(Some(1), None, Some(2), Some('b'), List(Some('b'))),
    PDARule(Some(2), Some('a'), Some(2), Some('a'), List()),
    PDARule(Some(2), Some('b'), Some(2), Some('b'), List()),
    PDARule(Some(2), None, Some(3), None, List(None))
  ))
  val configuration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
  val configurations: Set[PDAConfiguration[Int]] = Set(
    configuration
  )
  val npda: NPDA[Int] = NPDA(configurations, Set(Some(3)), rulebook)

  "current" should "return configurations free moved" in {
    npda.current should be (Set(
      PDAConfiguration(Some(1), List(None)),
      PDAConfiguration(Some(2), List(None)),
      PDAConfiguration(Some(3), List(None))
    ))
  }

  "isAccepting" should "return if current state is accepted or not" in {
    npda.isAccepting should be (true)
  }

  "readCharacter" should "return next NPDA whitch have a character" in {
    npda.readCharacter(Some('a')) should be (
      NPDA(
        Set(
          PDAConfiguration(Some(1), List(Some('a'), None)),
          PDAConfiguration(Some(2), List(Some('a'), None))
        ),
        Set[Option[Int]](Some(3)),
        rulebook
      )
    )
  }

}