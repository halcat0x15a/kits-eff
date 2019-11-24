package kits.eff

sealed abstract class Exc[E] extends Product with Serializable

object Exc {
  def raise[E](e: E)(implicit tag: Manifest[E]): Eff[Exc[E], Nothing] = Eff(Raise(tag, e))

  def lift[E: Manifest, A](either: Either[E, A]): Eff[Exc[E], A] = either.fold(raise(_), Eff.Pure(_))

  def run[E, R, A](eff: Eff[Exc[E] with R, A])(implicit tag: Manifest[E]): Eff[R, Either[E, A]] = {
    val handle = new Interpreter[Exc[E], R, A, Either[E, A]] {
      def pure(a: A) = Eff.Pure(Right(a))
      def flatMap[T](k: T => Eff[R, Either[E, A]]) = {
        case raise: Raise[E] if raise.tag == tag => Eff.Pure(Left(raise.value))
      }
    }
    handle(eff)
  }

  def accumulate[E, R, A](eff: Eff[Exc[E] with R, A])(implicit tag: Manifest[E]): Eff[R, Either[List[E], A]] = {
    val handle = new ApplicativeInterpreter[Exc[E], R] {
      type Result[A] = Either[List[E], A]
      def pure[A](a: A) = Eff.Pure(Right(a))
      def flatMap[A, B](k: A => Eff[R, Either[List[E], B]]) = {
        case raise: Raise[E] if raise.tag == tag => Eff.Pure(Left(List(raise.value)))
      }
      def ap[A, B](k: Eff[R, Either[List[E], A => B]]) = {
        case raise: Raise[E] if raise.tag == tag => k.map {
          case Left(es) => Left(raise.value :: es)
          case Right(_) => Left(List(raise.value))
        }
      }
      def functor[A, B](fa: Either[List[E], A])(f: A => B) = fa.map(f)
    }
    handle(eff)
  }

  def recover[E: Manifest, R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A]): Eff[R, A] = {
    run[E, R, A](eff).flatMap {
      case Right(a) => Eff.Pure(a)
      case Left(e) => f(e)
    }
  }

  case class Raise[E](tag: Manifest[E], value: E) extends Exc[E] with Fx[Nothing]

  def apply[E](implicit E: Manifest[E]): Ops[E] = new Ops(E)

  class Ops[E](val manifest: Manifest[E]) extends AnyVal {
    def run[R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[E, A]] = Exc.run[E, R, A](eff)(manifest)
    def recover[R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A]): Eff[R, A] = Exc.recover[E, R, A](eff)(f)(manifest)
  }
}
