package kits.eff

sealed abstract class Reader[I] extends Product with Serializable

object Reader {
  def ask[I](implicit tag: Manifest[I]): Eff[Reader[I], I] = Eff(Ask(tag))

  def run[I, R, A](i: I)(eff: Eff[Reader[I] with R, A])(implicit tag: Manifest[I]): Eff[R, A] = {
    val handle = new Recurser[Reader[I], R, A, A] {
      def pure(a: A) = Eff.Pure(a)
      def tailRec[T] = {
        case ask: Ask[I] if ask.tag == tag => Left(i)
      }
    }
    handle(eff)
  }

  def local[I: Manifest, R, A](f: I => I)(eff: Eff[Reader[I] with R, A]): Eff[Reader[I] with R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run[I, R, A](r)(eff)
    }

  case class Ask[I](tag: Manifest[I]) extends Reader[I] with Fx[I]
}
