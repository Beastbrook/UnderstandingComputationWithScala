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

}