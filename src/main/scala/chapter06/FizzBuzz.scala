package uc.lambda

object FizzBuzz {

  // Main fizzbuzz program
  def make: List[String] =
    (0 to 100).toList
      .map {
        case n if n % 15 == 0 => "FizzBuzz"
        case n if n % 3  == 0 => "Fizz"
        case n if n % 5  == 0 => "Buzz"
        case n => n.toString
      }

  // type definition
  type FBInt  = (Int => Int) => (Int => Int)
  type FBBool = Any => Any => Any

  // Number
  val ZERO: FBInt    = (p: (Int => Int)) => { (x: Int) => x }
  val ONE: FBInt     = (p: (Int => Int)) => { (x: Int) => p(x) }
  val TWO: FBInt     = (p: (Int => Int)) => { (x: Int) => p(p(x)) }
  val THREE: FBInt   = (p: (Int => Int)) => { (x: Int) => p(p(p(x))) }
  val FIVE: FBInt    = (p: (Int => Int)) => { (x: Int) => p(p(p(p(p(x))))) }
  val FIFTEEN: FBInt = (p: (Int => Int)) => { (x: Int) => p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(x))))))))))))))) }
  val HANDRED: FBInt = (p: (Int => Int)) => { (x: Int) =>
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
  val TRUE: FBBool  = (x: Any) => { (y: Any) => { x } }
  val FALSE: FBBool = (x: Any) => { (y: Any) => { y } }

  // Selection
  val IF: FBBool => Any => Any => Any =
    (bool: FBBool) => {
      (a: Any) => {
        (b: Any) => bool(a)(b)
      }
    }

  // converters
  def toInt(f: (Int => Int) => (Int => Int)): Int =
    f(_ + 1)(0)
  def toBoolean(f: Any => Any => Any): Boolean =
    f(true)(false).asInstanceOf[Boolean]

}