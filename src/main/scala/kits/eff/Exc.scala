package kits.eff

sealed abstract class Exc[E] extends Product with Serializable

object Exc {
  def fail[E: Manifest](e: E): Eff[Exc[E], Nothing] = Eff(Fail(e))

  def lift[E: Manifest, A](either: Either[E, A]): Eff[Exc[E], A] = either.fold(fail(_), Eff.Pure(_))

  def run[E: Manifest, R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[List[E], A]] = {
    val handle = new ApplicativeInterpreter[Exc[E], R] {
      type Result[A] = Either[List[E], A]
      def pure[A](a: A) = Eff.Pure(Right(a))
      def flatMap[A, B](fa: Exc[E] with Fx[A])(k: A => Eff[R, Either[List[E], B]]) =
        fa match {
          case Fail(e) => Eff.Pure(Left(List(e)))
        }
      def ap[A, B](fa: Exc[E] with Fx[A])(k: Eff[R, Either[List[E], A => B]]) =
        fa match {
          case Fail(e) => k.map {
            case Left(es) => Left(e :: es)
            case Right(_) => Left(List(e))
          }
        }
      def map[A, B](fa: Either[List[E], A])(f: A => B) = fa.map(f)
    }
    handle(eff)
  }

  def recover[E: Manifest, R, A](eff: Eff[Exc[E] with R, A])(f: List[E] => Eff[R, A]): Eff[R, A] = {
    run[E, R, A](eff).flatMap {
      case Right(a) => Eff.Pure(a)
      case Left(es) => f(es)
    }
  }

  case class Fail[E](value: E) extends Exc[E] with Fx[Nothing]

  def apply[E](implicit E: Manifest[E]): Ops[E] = new Ops(E)

  class Ops[E](val manifest: Manifest[E]) extends AnyVal {
    def run[R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[List[E], A]] = Exc.run[E, R, A](eff)(manifest)
    def recover[R, A](eff: Eff[Exc[E] with R, A])(f: List[E] => Eff[R, A]): Eff[R, A] = Exc.recover[E, R, A](eff)(f)(manifest)
  }
}
