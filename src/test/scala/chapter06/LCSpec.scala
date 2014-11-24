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

  "LC" should "replace variable with body" in {
    val expression1: LC = LCVariable("x")
    expression1.replace("x", LCFunction("y", LCVariable("y"))).toString should be ("-> y { y }")
    expression1.replace("z", LCFunction("y", LCVariable("y"))).toString should be ("x")

    val expression2: LC = LCCall(
      LCCall(
        LCCall(
          LCVariable("a"),
          LCVariable("b")
        ),
        LCVariable("c")
      ),
      LCVariable("b")
    )
    expression2.toString should be ("a[b][c][b]")
    expression2.replace("a", LCVariable("x")).toString should be ("x[b][c][b]")
    expression2.replace("b", LCFunction("x", LCVariable("x"))).toString should be ("a[-> x { x }][c][-> x { x }]")

    val expression3: LC = LCFunction("y", LCCall(LCVariable("x"), LCVariable("y")))
    expression3.toString should be ("-> y { x[y] }")
    expression3.replace("x", LCVariable("z")).toString should be ("-> y { z[y] }")
    expression3.replace("y", LCVariable("z")).toString should be ("-> y { x[y] }")

    val expression4: LC = LCCall(
      LCCall(LCVariable("x"), LCVariable("y")),
      LCFunction("y", LCCall(LCVariable("y"), LCVariable("x")))
    )
    expression4.toString should be ("x[y][-> y { y[x] }]")
    expression4.replace("x", LCVariable("z")).toString should be ("z[y][-> y { y[z] }]")
    expression4.replace("y", LCVariable("z")).toString should be ("x[z][-> y { y[x] }]")
  }

}