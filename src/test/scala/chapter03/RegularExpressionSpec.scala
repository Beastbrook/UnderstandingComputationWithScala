package uc.regularexpression

import org.scalatest._

class RegularExpressionSpec extends FlatSpec with Matchers {

  "Pattern" should "make AST of regular-expression" in {
    val pattern: Pattern = Repeat(
      Choose(
        Concatenate(Literal('a'), Literal('b')),
        Literal('a')
      )
    )
    pattern.inspect should be ("/(ab|a)*/")
  }

  "Empty" should "matches ''" in {
    val pattern: Pattern = Empty()
    pattern.matches("a") should be (false)
    pattern.matches("") should be (true)
  }

  "Literal" should "matches a single ascii" in {
    val pattern: Pattern = Literal('a')
    pattern.matches("a") should be (true)
    pattern.matches("aa") should be (false)
    pattern.matches("") should be (false)
    pattern.matches("b") should be (false)
  }

  "Concatenate" should "matches sequence of two Patterns" in {
    val pattern: Pattern = Concatenate(
      Literal('a'),
      Concatenate(Literal('b'), Literal('c'))
    )
    pattern.matches("a") should be (false)
    pattern.matches("ab") should be (false)
    pattern.matches("abc") should be (true)
    pattern.matches("abca") should be (false)
    val pattern2: Pattern = Concatenate(Literal('a'), Empty())
    pattern2.matches("a") should be (true)
    pattern2.matches("") should be (false)
  }

  "Choose" should "matches selection" in {
    val pattern: Pattern = Choose(
      Literal('a'),
      Literal('b')
    )
    pattern.matches("a") should be (true)
    pattern.matches("b") should be (true)
    pattern.matches("c") should be (false)
    val pattern2: Pattern = Concatenate(
      Choose(Literal('a'), Literal('b')),
      Literal('c')
    )
    pattern2.matches("ac") should be (true)
    pattern2.matches("bc") should be (true)
    pattern2.matches("abc") should be (false)
  }

  "Repeat" should "match at any times" in {
    val pattern: Pattern = Repeat(Literal('a'))
    pattern.matches("") should be (true)
    pattern.matches("a") should be (true)
    pattern.matches("aa") should be (true)
    pattern.matches("aab") should be (false)
    val pattern2: Pattern = Repeat(Concatenate(Literal('a'), Literal('b')))
    pattern2.matches("") should be (true)
    pattern2.matches("ab") should be (true)
    pattern2.matches("abab") should be (true)
    pattern2.matches("ababa") should be (false)
  }

}