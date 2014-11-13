package uc.lambda

import org.scalatest._

class LambdaSpec extends FlatSpec with Matchers {

  import Lambda._

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

  "ADD" should "return added value" in {
    toInt(ADD(ONE)(INCREMENT(ONE))) should be (3)
  }
  "SUBTRACT" should "return subtracted value" in {
    toInt(SUBTRACT(HANDRED)(ONE)) should be (99)
  }
  "MULTIPLY" should "return multiplied value" in {
    toInt(MULTIPLY(TWO)(THREE)) should be (6)
  }
  "POWER" should "return powered value" in {
    toInt(POWER(TWO)(THREE)) should be (8)
  }
  "MOD" should "return modular value" in {
    toInt(MOD(HANDRED)(THREE)) should be (1)
    toInt(MOD(FIVE)(FIVE)) should be (0)
    toInt(MOD(THREE)(FIVE)) should be (3)
  }
  "DIV" should "divide value" in {
    toInt(DIV(TEN)(TWO)) should be (5)
  }

  "IS_LESS_OR_EQUAL" should "return if A value is less than or equal B or not" in {
    toBoolean(IS_LESS_OR_EQUAL(ONE)(TWO)) should be (true)
    toBoolean(IS_LESS_OR_EQUAL(ONE)(ONE)) should be (true)
    toBoolean(IS_LESS_OR_EQUAL(TWO)(ONE)) should be (false)
  }

  "LIST" should "contain values" in {
    val myList = UNSHIFT(
      UNSHIFT(
        UNSHIFT(EMPTY)(THREE)
      )(FIVE)
    )(HANDRED)
    toInt(FIRST(myList)) should be (100)
    toInt(FIRST(REST(myList))) should be (5)
    toInt(FIRST(REST(REST(myList)))) should be (3)
    toBoolean(IS_EMPTY(EMPTY)) should be (true)
    toBoolean(IS_EMPTY(myList)) should be (false)

    toList(myList).map(toInt) should be (List(100, 5, 3))
  }

  "RANGE" should "create a list" in {
    val range: FBPair = RANGE(ONE)(FIVE).asInstanceOf[FBPair]
    toList(range).map(toInt) should be (List(1, 2, 3, 4, 5))
  }

  "FOLD" should "traverse list" in {
    toInt( FOLD(RANGE(ONE)(FIVE).asInstanceOf[FBPair])(ZERO)(ADD) ) should be (15)
  }

  "MAP" should "return new List" in {
    val myList = MAP(RANGE(ONE)(FIVE))(INCREMENT)
    toList(myList.asInstanceOf[FBPair]).map(toInt) should be (List(2, 3, 4, 5, 6))
  }

  "PUSH" should "add element at tail of list" in {
    val myList = RANGE(ONE)(FIVE).asInstanceOf[FBPair]
    toStr(PUSH(myList)(B).asInstanceOf[FBString]) should be ("12345B")
  }

  "FBString" should "be abe to convert to String" in {
    toChar(ZERO) should be ('0')
    toChar(B) should be ('B')
    toChar(ZED) should be ('z')
    toList(BUZZ).map(toChar) should be (List('B', 'u', 'z', 'z'))
    toStr(BUZZ) should be ("Buzz")
    toStr(FIZZ) should be ("Fizz")
    toStr(FIZZBUZZ) should be ("FizzBuzz")
  }

  "TO_DIGITS" should "convert 125 to List(1, 2, 5)" in {
    val n = TO_DIGITS(POWER(FIVE)(THREE)).asInstanceOf[FBString]
    toStr(n) should be ("125")
  }

}