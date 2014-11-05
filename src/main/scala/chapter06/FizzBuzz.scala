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

  // Number
  val ZERO    = (p: (Int => Int)) => { (x: Int) => x }
  val ONE     = (p: (Int => Int)) => { (x: Int) => p(x) }
  val TWO     = (p: (Int => Int)) => { (x: Int) => p(p(x)) }
  val THREE   = (p: (Int => Int)) => { (x: Int) => p(p(p(x))) }
  val FIVE    = (p: (Int => Int)) => { (x: Int) => p(p(p(p(p(x))))) }
  val FIFTEEN = (p: (Int => Int)) => { (x: Int) => p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(x))))))))))))))) }
  val HANDRED = (p: (Int => Int)) => { (x: Int) =>
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
    p(p(p(p(p(p(p(p(p(p(
      x
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
    ))))))))))
  }

  // Boolean
  val TRUE  = (x: Boolean) => { (y: Boolean) => { x } }
  val FALSE = (x: Boolean) => { (y: Boolean) => { y } }

  // converters
  def toInt(f: (Int => Int) => (Int => Int)): Int =
    f(_ + 1)(0)
  def toBoolean(f: Boolean => Boolean => Boolean): Boolean =
    f(true)(false)

}