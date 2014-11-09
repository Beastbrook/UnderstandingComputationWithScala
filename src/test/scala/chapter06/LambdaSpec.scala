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

  "IS_LESS_OR_EQUAL" should "return if A value is less than or equal B or not" in {
    toBoolean(IS_LESS_OR_EQUAL(ONE)(TWO)) should be (true)
    toBoolean(IS_LESS_OR_EQUAL(ONE)(ONE)) should be (true)
    toBoolean(IS_LESS_OR_EQUAL(TWO)(ONE)) should be (false)
  }

}