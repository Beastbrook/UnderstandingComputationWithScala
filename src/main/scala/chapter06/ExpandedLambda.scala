package uc.lambda

object ExpandedLambda {

  import Lambda._

  // stream
  val ZEROS: FBPair = UNSHIFT(ZEROS(_: Any => Any => Any))(ZERO)
  val UPWARDS_OF: Any => FBPair =
    (x: Any) => {
      UNSHIFT(UPWARDS_OF(INCREMENT(x))(_: Any => Any => Any))(x)
    }

  // converter
  def slice(stream: Any, n: Int): List[Any] =
    if (n < 1) Nil
    else FIRST(stream.asInstanceOf[FBPair]) :: slice(REST(stream.asInstanceOf[FBPair]), n - 1)

}