package kits.eff

trait Reader[I] {
  sealed abstract class Effect extends Product with Serializable

  case object Ask extends Effect with Fx[I]

  def ask: Eff[Effect, I] = Eff(Ask)

  def run[R, A](i: I)(eff: Eff[Effect with R, A]): Eff[R, A] = {
    val handle = new Recurser[Effect, R, A, A] {
      def pure(a: A) = Eff.Pure(a)
      def tailRec[T] = {
        case Ask => Left(i)
      }
    }
    handle(eff)
  }

  def local[R, A](eff: Eff[R, A])(f: I => I): Eff[Effect with R, A] =
    ask.flatMap { r0 =>
      val r = f(r0)
      run[R, A](r)(eff)
    }
}

object Reader {
  def apply[I](implicit reader: Reader[I]): reader.type = reader

  def ask[I](implicit reader: Reader[I]): Eff[reader.Effect, I] = reader.ask
}
