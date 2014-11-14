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

}