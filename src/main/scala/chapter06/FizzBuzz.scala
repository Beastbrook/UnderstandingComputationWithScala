package uc.lambda

object FizzBuzz {

  import Lambda._

  // Main fizzbuzz program
  def make: List[String] =
    (toInt(ZERO) to toInt(HANDRED))
      .toList
      .map((n: Int) => {
        IF(toFBBool(n % 15 == 0))("FizzBuzz")(
          IF(toFBBool(n % 3  == 0))("Fizz")(
            IF(toFBBool(n % 5  == 0))("Buzz")(n.toString)
          )
        ).asInstanceOf[String]
      })

}