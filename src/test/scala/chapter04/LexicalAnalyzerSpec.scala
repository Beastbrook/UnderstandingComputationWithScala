package uc.lexical

import org.scalatest._

class LexicalAnalyzerSpec extends FlatSpec with Matchers {

  "moreTokens" should "return if string is not empty" in {
    LexicalAnalyzer.moreTokens("") should be (false)
    LexicalAnalyzer.moreTokens("a") should be (true)
  }

  "nextStep" should "return (token, rest)" in {
    val string: String = "if (true) { a = 3 }"
    val expected: (Char, String) = ('i', " (true) { a = 3 }")
    LexicalAnalyzer.nextStep(string) should be (expected)
  }

  "analyze" should "parse string to tokens" in {
    val string1: String = "y = x * 7"
    LexicalAnalyzer.analyze(string1) should be (List('v', '=', 'v', '*', 'n'))
    val string2: String = "while (x < 5) { x = x * 3}"
    LexicalAnalyzer.analyze(string2) should be (List(
      'w', '(', 'v', '<', 'n', ')', '{', 'v', '=', 'v', '*', 'n', '}'
    ))
    val string3: String = "if(x<10){y=true;x=0} else{do-nothing}"
    LexicalAnalyzer.analyze(string3) should be (List(
      'i', '(', 'v', '<', 'n', ')', '{', 'v', '=', 'b', ';', 'v', '=', 'n', '}', 'e', '{', 'd', '}'
    ))
  }

}