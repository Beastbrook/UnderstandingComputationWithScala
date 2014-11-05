package uc.lambda

object FizzBuzz {

  def make: List[String] =
    (0 to 100).toList
      .map {
        case n if n % 15 == 0 => "FizzBuzz"
        case n if n % 3  == 0 => "Fizz"
        case n if n % 5  == 0 => "Buzz"
        case n => n.toString
      }

}