package uc.lambda.lc

import org.scalatest._

class LCSpec extends FlatSpec with Matchers {

  "LC" should "convert to Ruby lambda" in {
    val one = LCFunction("p",
      LCFunction("x",
        LCCall(LCVariable("p"), LCVariable("x"))
      )
    )
    one.toString should be ("-> p { -> x { p[x] } }")

    val increment =
      LCFunction("n",
        LCFunction("p",
          LCFunction("x",
            LCCall(
              LCVariable("p"),
              LCCall(
                LCCall(LCVariable("n"), LCVariable("p")),
                LCVariable("x")
              )
            )
          )
        )
      )
    increment.toString should be ("-> n { -> p { -> x { p[n[p][x]] } } }")

    val add =
      LCFunction("m",
        LCFunction("n",
          LCCall(
            LCCall(LCVariable("n"), increment),
            LCVariable("m")
          )
        )
      )
    add.toString should be ("-> m { -> n { n[-> n { -> p { -> x { p[n[p][x]] } } }][m] } }")
  }

}