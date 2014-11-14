package uc.lambda

object ExpandedLambda {

  import Lambda._

  // stream
  val ZEROS: FBPair = UNSHIFT(ZEROS(_: Any => Any => Any))(ZERO)
  val UPWARDS_OF: Any => FBPair =
    (x: Any) => {
      UNSHIFT(UPWARDS_OF(INCREMENT(x))(_: Any => Any => Any))(x)
    }
  val _MULTIPLES_OF: Any => Any => FBPair =
    (m: Any) => {
      (n: Any) => {
        UNSHIFT( _MULTIPLES_OF(ADD(n)(m))(n)(_: Any => Any => Any) )(m)
      }
    }
  val MULTIPLES_OF: Any => FBPair =
    (x: Any) => _MULTIPLES_OF(x)(x)
  val MULTIPLY_STREAM: FBPair => FBPair => FBPair =
    (stream1: FBPair) => {
      (stream2: FBPair) => {
        UNSHIFT( MULTIPLY_STREAM(REST(stream1))(REST(stream2))(_: Any => Any => Any) )(MULTIPLY(FIRST(stream1))(FIRST(stream2)))
      }
    }

  // converter
  def slice(stream: Any, n: Int): List[Any] =
    if (n < 1) Nil
    else FIRST(stream.asInstanceOf[FBPair]) :: slice(REST(stream.asInstanceOf[FBPair]), n - 1)

}