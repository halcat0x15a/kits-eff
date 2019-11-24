package kits.eff

object Opt {
  def empty: Eff[Opt, Nothing] = Exc.raise(())

  def lift[A](option: Option[A]): Eff[Opt, A] =
    option match {
      case None => empty
      case Some(a) => Eff.Pure(a)
    }

  def run[R, A](eff: Eff[Opt with R, A]): Eff[R, Option[A]] = {
    val tag = implicitly[Manifest[Unit]]
    val handle = new Interpreter[Opt, R, A, Option[A]] {
      def pure(a: A) = Eff.Pure(Some(a))
      def flatMap[T](k: T => Eff[R, Option[A]]) = {
        case raise: Exc.Raise[Unit] if raise.tag == tag => Eff.Pure(None)
      }
    }
    handle(eff)
  }
}
