package uc.lambda

object Lambda {

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
    (p: Any) => PAIR(RIGHT(p.asInstanceOf[FBPair]))(INCREMENT(RIGHT(p.asInstanceOf[FBPair])))
  val INCREMENT: Any => Any =
    (n: Any) => {
      (p: Any => Any) => {
        (x: Any) => p(n.asInstanceOf[FBInt](p)(x))
      }
    }
  val DECREMENT: Any => Any =
    (n: Any) => LEFT( n.asInstanceOf[FBInt](SLIDE)(PAIR(ZERO)(ZERO)).asInstanceOf[FBPair] )

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

  // calc
  val ADD: Any => Any => Any =
    (m: Any) => {
      (n: Any) => {
        n.asInstanceOf[FBInt](INCREMENT)(m)
      }
    }
  val SUBTRACT: Any => Any => Any =
    (m: Any) => {
      (n: Any) => {
        n.asInstanceOf[FBInt](DECREMENT)(m)
      }
    }
  val MULTIPLY: Any => Any => Any =
    (m: Any) => {
      (n: Any) => {
        n.asInstanceOf[FBInt](ADD(m))(ZERO)
      }
    }
  val POWER: Any => Any => Any =
    (m: Any) => {
      (n: Any) => {
        n.asInstanceOf[FBInt](MULTIPLY(m))(ONE)
      }
    }

  // converters
  def toInt(f: Any): Int =
    f.asInstanceOf[FBInt](_.asInstanceOf[Int] + 1)(0).asInstanceOf[Int]
  def toBoolean(f: Any => Any => Any): Boolean =
    f(true)(false).asInstanceOf[Boolean]
  def toFBBool(b: Boolean): FBBool =
    if (b) TRUE else FALSE

}