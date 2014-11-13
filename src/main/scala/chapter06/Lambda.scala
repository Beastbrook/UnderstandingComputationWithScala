package uc.lambda

object Lambda {

  // type definition
  type FBInt  = (Any => Any) => (Any => Any)
  type FBBool = Any => Any => Any
  type FBPair = (Any => Any => Any) => Any

  // combinator
  val Z: ((Any => Any => Any) => (Any => Any => Any)) => (Any => Any => Any) =
    (f: (Any => Any => Any) => (Any => Any => Any)) => {
      (x: Any) => f(Z(f))(x)
    }
  val Z2: ((Any => Any => (Any => Any => Any) => Any) => (Any => Any => (Any => Any => Any) => Any)) => (Any => Any => (Any => Any => Any) => Any) =
    (f: (Any => Any => (Any => Any => Any) => Any) => (Any => Any => (Any => Any => Any) => Any)) => {
      (x: Any) => f(Z2(f))(x)
    }

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

  // compare
  val IS_ZERO: Any => FBBool =
    (n: Any) => n.asInstanceOf[FBInt](_ => (FALSE))(TRUE).asInstanceOf[FBBool]
  val IS_LESS_OR_EQUAL: Any => Any => FBBool =
    (m: Any) => {
      (n: Any) => {
        IS_ZERO(SUBTRACT(m)(n))
      }
    }

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
  val DIV: Any => Any => Any =
    (m: Any) => {
      (n: Any) => {
        IF(IS_LESS_OR_EQUAL(n)(m))(
          INCREMENT(DIV(SUBTRACT(m)(n))(n).asInstanceOf[FBInt](_: Any => Any))
        )(
          ZERO
        )
      }
    }

  val MOD: Any => Any => Any =
    Z(
      (f: Any => Any => Any) => {
        (m: Any) => {
          (n: Any) => {
            IF(IS_LESS_OR_EQUAL(n)(m))(
              (x: Any => Any) => { f(SUBTRACT(m)(n))(n).asInstanceOf[FBInt](x) }
            )(
              m
            )
          }
        }
      }
    )

  // LIST
  val EMPTY: FBPair = PAIR(TRUE)(TRUE)
  val UNSHIFT: FBPair => Any => FBPair = (l: FBPair) => { (x: Any) =>
    PAIR(FALSE)(PAIR(x)(l))
  }
  val IS_EMPTY: FBPair => Any = LEFT
  val FIRST: FBPair => Any = (l: FBPair) => {
    LEFT(RIGHT(l).asInstanceOf[FBPair])
  }
  val REST: FBPair => FBPair = (l: FBPair) => {
    RIGHT(RIGHT(l).asInstanceOf[FBPair]).asInstanceOf[FBPair]
  }

  // LIST utility
  val FOLD: Any => Any => (Any => Any => Any) => Any =
    Z2(
      (f: Any => Any => (Any => Any => Any) => Any) => {
        (list: Any) => {
          (x: Any) => {
            (g: Any => Any => Any) => {
              IF( IS_EMPTY(list.asInstanceOf[FBPair]).asInstanceOf[FBBool] )(
                x
              )(
                g(
                  f(
                    REST(list.asInstanceOf[FBPair])
                  )(
                    x
                  )(
                    g
                  ).asInstanceOf[FBInt](_: Any => Any)
                )(
                  FIRST(list.asInstanceOf[FBPair])
                )
              )
            }
          }
        }
      }
    )

  val MAP: Any => (Any => Any) => Any =
    (list: Any) => {
      (f: Any => Any) => {
        IF(IS_EMPTY(list.asInstanceOf[FBPair]).asInstanceOf[FBBool])(
          EMPTY
        )(
          UNSHIFT( MAP(REST(list.asInstanceOf[FBPair]))(f).asInstanceOf[FBPair](_: Any => Any => Any) )( f(FIRST(list.asInstanceOf[FBPair])) )
        )
      }
    }

  val PUSH: Any => Any => Any =
    (list: Any) => {
      (x: Any) => {
        IF(IS_EMPTY(list.asInstanceOf[FBPair]).asInstanceOf[FBBool])(
          UNSHIFT(EMPTY)(x)
        )(
          UNSHIFT(
            PUSH( REST(list.asInstanceOf[FBPair]) )(x).asInstanceOf[FBPair](_: Any => Any => Any)
          )(
            FIRST(list.asInstanceOf[FBPair])
          )
        )
      }
    }

  // Range
  val RANGE: Any => Any => Any =
    Z(
      (f: Any => Any => Any) => {
        (m: Any) => {
          (n: Any) => {
            IF(IS_LESS_OR_EQUAL(m.asInstanceOf[FBInt])(n.asInstanceOf[FBInt]))(
              UNSHIFT( f(INCREMENT(m).asInstanceOf[FBInt])(n).asInstanceOf[FBPair] )(m).asInstanceOf[FBPair](_: Any => Any => Any)
            )(
              EMPTY
            )
          }
        }
      }
    )

  // String
  val TEN: FBInt = MULTIPLY(TWO)(FIVE).asInstanceOf[FBInt]
  val B: FBInt   = TEN
  val F: FBInt   = INCREMENT(B).asInstanceOf[FBInt]
  val I: FBInt   = INCREMENT(F).asInstanceOf[FBInt]
  val U: FBInt   = INCREMENT(I).asInstanceOf[FBInt]
  val ZED: FBInt = INCREMENT(U).asInstanceOf[FBInt]

  val FIZZ: FBPair = UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(EMPTY)(ZED))(ZED))(I))(F)
  val BUZZ: FBPair = UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(EMPTY)(ZED))(ZED))(U))(B)
  val FIZZBUZZ: FBPair = UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(UNSHIFT(EMPTY)(ZED))(ZED))(U))(B))(ZED))(ZED))(I))(F)

  val TO_DIGITS: Any => Any =
    (n: Any) => {
      PUSH(
        IF( IS_LESS_OR_EQUAL(n)(DECREMENT(TEN)).asInstanceOf[FBBool] )(
          EMPTY
        )(
          TO_DIGITS(DIV(n)(TEN)).asInstanceOf[FBPair](_: Any => Any => Any)
        )
      )(
        MOD(n)(TEN)
      )
    }

  // converters
  def toInt(f: Any): Int =
    f.asInstanceOf[FBInt](_.asInstanceOf[Int] + 1)(0).asInstanceOf[Int]
  def toBoolean(f: Any): Boolean =
    f.asInstanceOf[FBBool](true)(false).asInstanceOf[Boolean]
  def toFBBool(b: Boolean): FBBool =
    if (b) TRUE else FALSE
  def toList(l: FBPair): List[Any] = l match {
    case li if toBoolean(IS_EMPTY(li)) => Nil
    case li: FBPair => FIRST(li) :: toList(REST(li))
  }
  def toChar(c: Any): Char =
    "0123456789BFiuz".charAt(toInt(c))
  def toStr(str: Any): String =
    toList(str.asInstanceOf[FBPair]).map(toChar).mkString("", "", "")

}