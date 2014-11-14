package uc.lambda

import org.scalatest._

class ExpandedLambdaSpec extends FlatSpec with Matchers {

  import Lambda._
  import ExpandedLambda._

  "ZEROS" should "be stream of zero" in {
    toInt(FIRST(ZEROS)) should be (0)
    toInt(FIRST(REST(ZEROS.asInstanceOf[FBPair]).asInstanceOf[FBPair]).asInstanceOf[FBPair]) should be (0)
    slice(ZEROS, 5).map(toChar).mkString("", "", "") should be ("00000")
  }

  "UPWARDS_OF" should "return count down list" in {
    slice(UPWARDS_OF(ZERO), 5).map(toChar).mkString("", "", "") should be ("01234")
    slice(UPWARDS_OF(FIVE), 5).map(toChar).mkString("", "", "") should be ("56789")
  }

}