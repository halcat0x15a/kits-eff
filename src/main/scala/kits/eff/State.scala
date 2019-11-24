package kits.eff

object State {
  def get[S](implicit tag: Manifest[S]): Eff[State[S], S] = Eff(Reader.Ask(tag))

  def put[S](s: S)(implicit tag: Manifest[S]): Eff[State[S], Unit] = Eff(Writer.Tell(tag, s))

  def modify[S: Manifest](f: S => S): Eff[State[S], Unit] =
    get[S].flatMap(s => put(f(s)))

  def run[S, R, A](s: S)(eff: Eff[State[S] with R, A])(implicit tag: Manifest[S]): Eff[R, (S, A)] = {
    val handle = new StateRecurser[State[S], R, S, A, (S, A)] {
      def pure(s: S, a: A) = Eff.Pure((s, a))
      def tailRec[T](s: S) = {
        case ask: Reader.Ask[S] if ask.tag == tag => Left((s, s))
        case tell: Writer.Tell[S] if tell.tag == tag => Left((tell.value, ()))
      }
    }
    handle(s, eff)
  }

  def transaction[S: Manifest, R, A](eff: Eff[State[S] with R, A]): Eff[State[S] with R, A] =
    for {
      s <- get
      r <- run[S, R, A](s)(eff)
      _ <- put(r._1)
    } yield r._2

  def apply[S](implicit S: Manifest[S]) = new Ops(S)

  class Ops[S](val manifest: Manifest[S]) extends AnyVal {
    def transaction[R, A](eff: Eff[State[S] with R, A]): Eff[State[S] with R, A] = State.transaction[S, R, A](eff)(manifest)
  }
}
