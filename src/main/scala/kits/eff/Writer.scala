package kits.eff

trait Writer[W] {
  sealed abstract class Effect extends Product with Serializable

  case class Tell(value: W) extends Effect with Fx[Unit]

  def tell(w: W): Eff[Effect, Unit] = Eff(Tell(w))

  def fold[R, A, B](eff: Eff[Effect with R, A])(z: B)(f: (B, W) => B): Eff[R, (B, A)] = {
    val handle = new StateRecurser[Effect, R, B, A, (B, A)] {
      def pure(s: B, a: A) = Eff.Pure((s, a))
      def tailRec[T](s: B) = {
        case Tell(w) => Left(f(s, w), ())
      }
    }
    handle(z, eff)
  }

  def run[R, A](eff: Eff[Effect with R, A]): Eff[R, (Vector[W], A)] =
    fold[R, A, Vector[W]](eff)(Vector.empty[W])(_ :+ _)

  def listen[R, A](eff: Eff[R, A]): Eff[Effect with R, (Vector[W], A)] =
    for {
      r <- run[R, A](eff)
      _ <- r._1.foldLeft(Eff.Pure(()): Eff[Effect with R, Unit]) { (eff, w) =>
        eff.flatMap(_ => tell(w))
      }
    } yield r
}

object Writer {
  def apply[W](implicit writer: Writer[W]): writer.type = writer

  def tell[W](w: W)(implicit writer: Writer[W]): Eff[writer.Effect, Unit] = writer.tell(w)
}
