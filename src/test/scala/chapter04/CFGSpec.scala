package uc.test

import uc.pda._
import uc.lexical._

import scala.util.matching.Regex

import org.scalatest._

class CFGSpec extends FlatSpec with Matchers {

  val rules: Set[PDARule[Int]] = Set[PDARule[Int]](

    // start rule
    PDARule(Some(1), None, Some(2), None, List(Some('S'), None)),

    // <statement> ::= <while> | <assign>
    PDARule(Some(2), None, Some(2), Some('S'), List(Some('W'))),
    PDARule(Some(2), None, Some(2), Some('S'), List(Some('A'))),

    // <while> ::= 'w' '(' <expression> ')' '{' <statement> '}'
    PDARule(Some(2), None, Some(2), Some('W'),
      List(Some('w'), Some('('), Some('E'), Some(')'), Some('{'), Some('S'), Some('}'))),

    // <assign> ::= 'v' '=' <expression>
    PDARule(Some(2), None, Some(2), Some('A'),
      List(Some('v'), Some('='), Some('E'))),

    // <expression> ::= <less-than>
    PDARule(Some(2), None, Some(2), Some('E'), List(Some('L'))),

    // <less-than> ::= <multiply> '<' <less-than> | <multiply>
    PDARule(Some(2), None, Some(2), Some('L'), List(Some('M'), Some('<'), Some('L'))),
    PDARule(Some(2), None, Some(2), Some('L'), List(Some('M'))),

    // <multiply> ::= <term> '*' <multiply> | <term>
    PDARule(Some(2), None, Some(2), Some('M'), List(Some('T'), Some('*'), Some('M'))),
    PDARule(Some(2), None, Some(2), Some('M'), List(Some('T'))),

    // <term> ::= 'n' | 'v'
    PDARule(Some(2), None, Some(2), Some('T'), List(Some('n'))),
    PDARule(Some(2), None, Some(2), Some('T'), List(Some('v')))

  ) | LexicalAnalyzer.GRAMMAR.map((rule: (Char, Regex)) =>
    PDARule(Some(2), Some(rule._1), Some(2), Some(rule._1), List())
  ).toSet | Set(
    PDARule(Some(2), None, Some(3), None, List(None))
  )

  val startConfiguration: PDAConfiguration[Int] = PDAConfiguration(Some(1), List(None))
  val acceptCunfigurations: Set[Option[Int]] = Set(Some(3))

  "NPDA" should "be able to create cfg" in {
    val npda: NPDA[Int] = NPDA(Set(startConfiguration), acceptCunfigurations, NPDARulebook(rules))
    npda.accepts(LexicalAnalyzer.analyze("while (x < 5) { x = x * 3 }").mkString("", "", "")) should be (true)
    npda.accepts(LexicalAnalyzer.analyze("while (x < 5 x = x * }").mkString("", "", "")) should be (false)
  }

}