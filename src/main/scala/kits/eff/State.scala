package kits.eff

trait State[S] {
  val reader: Reader[S] = new Reader[S] {}

  val writer: Writer[S] = new Writer[S] {}

  type Effect = reader.Effect with writer.Effect

  def get: Eff[Effect, S] = reader.ask

  def put(s: S): Eff[Effect, Unit] = writer.tell(s)

  def modify(f: S => S): Eff[Effect, Unit] =
    get.flatMap(s => put(f(s)))

  def run[R, A](s: S)(eff: Eff[Effect with R, A]): Eff[R, (S, A)] = {
    val handle = new StateRecurser[Effect, R, S, A, (S, A)] {
      def pure(s: S, a: A) = Eff.Pure((s, a))
      def tailRec[T](s: S) = {
        case reader.Ask => Left(s, s)
        case writer.Tell(s) => Left(s, ())
      }
    }
    handle(s, eff)
  }

  def transaction[R, A](eff: Eff[R, A]): Eff[Effect with R, A] =
    for {
      s <- get
      r <- run[R, A](s)(eff)
      _ <- put(r._1)
    } yield r._2
}

object State {
  def apply[S](implicit state: State[S]): state.type = state

  def get[S](implicit state: State[S]): Eff[state.Effect, S] = state.get

  def put[S](s: S)(implicit state: State[S]): Eff[state.Effect, Unit] = state.put(s)

  def modify[S](f: S => S)(implicit state: State[S]): Eff[state.Effect, Unit] = state.modify(f)
}
