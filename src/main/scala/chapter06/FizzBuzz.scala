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
  type FBPair = (Any => Any => Any) => Any

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
  val SLIDE: Any => Any =
    (p: Any) => PAIR(RIGHT(p.asInstanceOf[FBPair]))(INCREMENT(RIGHT(p.asInstanceOf[FBPair]).asInstanceOf[FBInt]))
  val INCREMENT: FBInt => FBInt =
    (n: FBInt) => {
      (p: Any => Any) => {
        (x: Any) => p(n(p)(x))
      }
    }
  val DECREMENT: FBInt => FBInt =
    (n: FBInt) => LEFT( n(SLIDE)(PAIR(ZERO)(ZERO)).asInstanceOf[FBPair] ).asInstanceOf[FBInt]

  // Boolean
  val TRUE: FBBool  = (x: Any) => { (y: Any) => { x } }
  val FALSE: FBBool = (x: Any) => { (y: Any) => { y } }

  // Selection
  val IF: FBBool => Any => Any => Any =
    (bool: FBBool) => bool

  val IS_ZERO: FBInt => FBBool =
    (n: FBInt) => n(_ => (FALSE))(TRUE).asInstanceOf[FBBool]

  // pair
  val PAIR: Any => Any => FBPair =
    (x: Any) => {
      (y: Any) => {
        (f: Any => Any => Any) => f(x)(y)
      }
    }
  val LEFT: FBPair => Any =
    (p: FBPair) => p((x: Any) => {(y: Any) => x})
  val RIGHT: FBPair => Any =
    (p: FBPair) => p((x: Any) => {(y: Any) => y})

  // converters
  def toInt(f: FBInt): Int =
    f(_.asInstanceOf[Int] + 1)(0).asInstanceOf[Int]
  def toBoolean(f: Any => Any => Any): Boolean =
    f(true)(false).asInstanceOf[Boolean]
  def toFBBool(b: Boolean): FBBool =
    if (b) TRUE else FALSE

}