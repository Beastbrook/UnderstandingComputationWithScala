package uc.lambda

import org.scalatest._

class FizzBuzzSpec extends FlatSpec with Matchers {

  import FizzBuzz._

  val fizzbuzz: List[String] = make

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
    toInt(ZERO) should be (0)
    toInt(ONE) should be (1)
    toInt(TWO) should be (2)
    toInt(THREE) should be (3)
    toInt(FIVE) should be (5)
    toInt(FIFTEEN) should be (15)
    toInt(HANDRED) should be (100)
  }

  "TRUE,FALSE" should "be able to translate to boolean" in {
    toBoolean(TRUE) should be (true)
    toBoolean(FALSE) should be (false)
  }

  "IF" should "work like if" in {
    toInt(IF(TRUE)(ONE)(TWO).asInstanceOf[FBInt]) should be (1)
    toInt(IF(FALSE)(ONE)(TWO).asInstanceOf[FBInt]) should be (2)
    IF(TRUE)("happy")("sad").asInstanceOf[String] should be ("happy")
    IF(FALSE)("happy")("sad").asInstanceOf[String] should be ("sad")
  }

  "IS_ZERO" should "return true if n == 0" in {
    toBoolean(IS_ZERO(ZERO)) should be (true)
    toBoolean(IS_ZERO(ONE)) should be (false)
  }

  "PAIR" should "have two values" in {
    val myPair = PAIR(THREE)(FIVE)
    toInt(LEFT(myPair).asInstanceOf[FBInt]) should be (3)
    toInt(RIGHT(myPair).asInstanceOf[FBInt]) should be (5)
  }

  "INCREMENT" should "create n+1" in {
    toInt(INCREMENT(ZERO)) should be (1)
    toInt(INCREMENT(INCREMENT(ZERO))) should be (2)
    toInt(INCREMENT(HANDRED)) should be (101)
  }

  "DECREMENT" should "create n-1" in {
    toInt(DECREMENT(ONE)) should be (0)
    toInt(DECREMENT(HANDRED)) should be (99)
  }

}