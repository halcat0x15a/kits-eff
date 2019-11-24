package kits.eff

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def tell[W](w: W)(implicit tag: Manifest[W]): Eff[Writer[W], Unit] = Eff(Tell(tag, w))

  def fold[W, R, A, B](eff: Eff[Writer[W] with R, A])(z: B)(f: (B, W) => B)(implicit tag: Manifest[W]): Eff[R, (B, A)] = {
    val handle = new StateRecurser[Writer[W], R, B, A, (B, A)] {
      def pure(s: B, a: A) = Eff.Pure((s, a))
      def tailRec[T](s: B) = {
        case tell: Tell[W] if tell.tag == tag => Left(f(s, tell.value), ())
      }
    }
    handle(z, eff)
  }

  def run[W: Manifest, R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] =
    fold[W, R, A, Vector[W]](eff)(Vector.empty[W])(_ :+ _)

  def listen[W: Manifest, R, A](eff: Eff[Writer[W] with R, A]): Eff[Writer[W] with R, (Vector[W], A)] =
    for {
      r <- run[W, R, A](eff)
      _ <- r._1.foldLeft(Eff.Pure(()): Eff[Writer[W] with R, Unit]) { (eff, w) =>
        eff.flatMap(_ => tell(w))
      }
    } yield r

  case class Tell[W](tag: Manifest[W], value: W) extends Writer[W] with Fx[Unit]

  def apply[W](implicit W: Manifest[W]): Ops[W] = new Ops(W)

  class Ops[W](val manifest: Manifest[W]) extends AnyVal {
    def fold[R, A, B](eff: Eff[Writer[W] with R, A])(z: B)(f: (B, W) => B): Eff[R, (B, A)] = Writer.fold[W, R, A, B](eff)(z)(f)(manifest)
    def run[R, A](eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] = Writer.run[W, R, A](eff)(manifest)
    def listen[R, A](eff: Eff[Writer[W] with R, A]): Eff[Writer[W] with R, (Vector[W], A)] = Writer.listen[W, R, A](eff)(manifest)
  }
}
