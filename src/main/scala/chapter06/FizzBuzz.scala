package uc.lambda

object FizzBuzz {

  import Lambda._

  val CONVERT: Any => Any =
    (n: Any) => {
      IF( IS_LESS_OR_EQUAL( MOD(n)(FIFTEEN) )(ZERO) )(
        FIZZBUZZ
      )(
        IF( IS_LESS_OR_EQUAL( MOD(n)(THREE) )(ZERO) )(
          FIZZ
        )(
          IF( IS_LESS_OR_EQUAL( MOD(n)(FIVE) )(ZERO) )(
            BUZZ
          )(
            TO_DIGITS(n)
          )
        )
      )
    }

  // Main fizzbuzz program
  def make = {
    val fizzbuzz: FBPair = MAP(RANGE(ZERO)(HANDRED))(CONVERT).asInstanceOf[FBPair]
    toList(fizzbuzz)
      .map(toStr)
  }


}