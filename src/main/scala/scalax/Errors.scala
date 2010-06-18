package scalax

object Errors {
  def pack[A](body: => A): Either[Throwable, A] =
    try {
      Right(body)
    } catch {
      case e => Left(e)
    }

  def unpack[A](packed: Either[Throwable, A]) = packed match {
    case Left(e) => throw e
    case Right(v) => v
  }

  def unpack(packed: Any) = packed match {
    case Left(e: Throwable) => throw e
    case Right(v) => v
    case v => v
  }

  def ignore[A](types: Class[_ <: Throwable]*)(body: => A): Option[A] =
    (pack { body }) match {
      case Right(x) => Some(x)
      case Left(e) =>
        if (types.find(_.isAssignableFrom(e.getClass)).isDefined) None
        else throw e
    }

  def ignore[A](body: => A): Option[A] = (pack { body }).right.toOption

  def wrap[A](msg: String, clazz: Class[_ <: Exception])(body: => A): A = {
    try {
      body
    } catch {
      case e: Throwable =>
        val c = clazz.getConstructor(classOf[String], classOf[Throwable])
        throw c.newInstance(msg, e).asInstanceOf[Throwable]
    }
  }
}
