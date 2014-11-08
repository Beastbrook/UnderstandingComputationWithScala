package uc.lambda

import org.scalatest._

class FizzBuzzSpec extends FlatSpec with Matchers {

  val fizzbuzz: List[String] = FizzBuzz.make

  "FizzBuzz" should "be list of number, fizz, buzz and fizzbuzz" in {
    fizzbuzz(0) should be ("FizzBuzz")
    fizzbuzz(2) should be ("2")
    fizzbuzz(3) should be ("Fizz")
    fizzbuzz(5) should be ("Buzz")
    fizzbuzz(15) should be ("FizzBuzz")
    fizzbuzz(30) should be ("FizzBuzz")
    fizzbuzz(33) should be ("Fizz")
    fizzbuzz(95) should be ("Buzz")
  }

  "ONE,TWO,THREE" should "be able to translate to number" in {
    FizzBuzz.toInt(FizzBuzz.ZERO) should be (0)
    FizzBuzz.toInt(FizzBuzz.ONE) should be (1)
    FizzBuzz.toInt(FizzBuzz.TWO) should be (2)
    FizzBuzz.toInt(FizzBuzz.THREE) should be (3)
    FizzBuzz.toInt(FizzBuzz.FIVE) should be (5)
    FizzBuzz.toInt(FizzBuzz.FIFTEEN) should be (15)
    FizzBuzz.toInt(FizzBuzz.HANDRED) should be (100)
  }

  "TRUE,FALSE" should "be able to translate to boolean" in {
    FizzBuzz.toBoolean(FizzBuzz.TRUE) should be (true)
    FizzBuzz.toBoolean(FizzBuzz.FALSE) should be (false)
  }

  "IF" should "work like if" in {
    FizzBuzz.toInt(FizzBuzz.IF(FizzBuzz.TRUE)(FizzBuzz.ONE)(FizzBuzz.TWO).asInstanceOf[FizzBuzz.FBInt]) should be (1)
    FizzBuzz.toInt(FizzBuzz.IF(FizzBuzz.FALSE)(FizzBuzz.ONE)(FizzBuzz.TWO).asInstanceOf[FizzBuzz.FBInt]) should be (2)
    FizzBuzz.IF(FizzBuzz.TRUE)("happy")("sad").asInstanceOf[String] should be ("happy")
    FizzBuzz.IF(FizzBuzz.FALSE)("happy")("sad").asInstanceOf[String] should be ("sad")
  }

  "IS_ZERO" should "return true if n == 0" in {
    FizzBuzz.toBoolean(FizzBuzz.IS_ZERO(FizzBuzz.ZERO)) should be (true)
    FizzBuzz.toBoolean(FizzBuzz.IS_ZERO(FizzBuzz.ONE)) should be (false)
  }

}