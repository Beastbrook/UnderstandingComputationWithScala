package uc.lambda

object FizzBuzz {

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

  // type definition
  type FBInt  = (Any => Any) => (Any => Any)
  type FBBool = Any => Any => Any

  // Number
  val ZERO: FBInt    = (p: (Any => Any)) => { (x: Any) => x }
  val ONE: FBInt     = (p: (Any => Any)) => { (x: Any) => p(x) }
  val TWO: FBInt     = (p: (Any => Any)) => { (x: Any) => p(p(x)) }
  val THREE: FBInt   = (p: (Any => Any)) => { (x: Any) => p(p(p(x))) }
  val FIVE: FBInt    = (p: (Any => Any)) => { (x: Any) => p(p(p(p(p(x))))) }
  val FIFTEEN: FBInt = (p: (Any => Any)) => { (x: Any) => p(p(p(p(p(p(p(p(p(p(p(p(p(p(p(x))))))))))))))) }
  val HANDRED: FBInt = (p: (Any => Any)) => { (x: Any) =>
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
    (bool: FBBool) => bool

  // converters
  def toInt(f: FBInt): Int =
    f(_.asInstanceOf[Int] + 1)(0).asInstanceOf[Int]
  def toBoolean(f: Any => Any => Any): Boolean =
    f(true)(false).asInstanceOf[Boolean]
  def toFBBool(b: Boolean): FBBool =
    if (b) TRUE else FALSE

}