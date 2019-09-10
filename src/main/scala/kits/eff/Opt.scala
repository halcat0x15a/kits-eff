package kits.eff

object Opt {
  val exc = new Exc[Unit] {}

  type Effect = exc.Effect

  def empty: Eff[Effect, Nothing] = exc.raise(())

  def lift[A](option: Option[A]): Eff[Effect, A] =
    option match {
      case None => empty
      case Some(a) => Eff.Pure(a)
    }

  def run[R, A](eff: Eff[Effect with R, A]): Eff[R, Option[A]] = {
    val handle = new Interpreter[Effect, R, A, Option[A]] {
      def pure(a: A) = Eff.Pure(Some(a))
      def flatMap[T](k: T => Eff[R, Option[A]]) = {
        case exc.Raise(()) => Eff.Pure(None)
      }
    }
    handle(eff)
  }
}
