package uc.lambda

import org.scalatest._

class FizzBuzzSpec extends FlatSpec with Matchers {

  import Lambda._
  import FizzBuzz._

  "CONVERT" should "n -> String" in {
    toStr(CONVERT(ZERO)) should be ("FizzBuzz")
    toStr(CONVERT(ONE)) should be ("1")
    toStr(CONVERT(TWO)) should be ("2")
    toStr(CONVERT(THREE)) should be ("Fizz")
    toStr(CONVERT(FIVE)) should be ("Buzz")
    toStr(CONVERT(INCREMENT(TEN))) should be ("11")
    toStr(CONVERT(HANDRED)) should be ("Buzz")
  }

  "FizzBuzz" should "be list of number, fizz, buzz and fizzbuzz" in {
    val fizzbuzz: List[String] = make
    fizzbuzz(2) should be ("2")
    fizzbuzz(3) should be ("Fizz")
    fizzbuzz(5) should be ("Buzz")
    fizzbuzz(15) should be ("FizzBuzz")
    fizzbuzz(30) should be ("FizzBuzz")
    fizzbuzz(33) should be ("Fizz")
    fizzbuzz(97) should be ("97")
    fizzbuzz(100) should be ("Buzz")
  }

}