package kits.eff

trait Exc[E] {
  sealed abstract class Effect extends Product with Serializable

  case class Raise(value: E) extends Effect with Fx[Nothing]

  def raise(e: E): Eff[Effect, Nothing] = Eff(Raise(e))

  def lift[A](either: Either[E, A]): Eff[Effect, A] = either.fold(raise(_), Eff.Pure(_))

  def run[R, A](eff: Eff[Effect with R, A]): Eff[R, Either[E, A]] = {
    val handle = new Interpreter[Effect, R, A, Either[E, A]] {
      def pure(a: A) = Eff.Pure(Right(a))
      def flatMap[T](k: T => Eff[R, Either[E, A]]) = {
        case Raise(e) => Eff.Pure(Left(e))
      }
    }
    handle(eff)
  }

  def accumulate[R, A](eff: Eff[Effect with R, A]): Eff[R, Either[List[E], A]] = {
    val handle = new ApplicativeInterpreter[Effect, R] {
      type Result[A] = Either[List[E], A]
      def pure[A](a: A) = Eff.Pure(Right(a))
      def flatMap[A, B](k: A => Eff[R, Either[List[E], B]]) = {
        case Raise(e) => Eff.Pure(Left(List(e)))
      }
      def ap[A, B](k: Eff[R, Either[List[E], A => B]]) = {
        case Raise(e) => k.map {
          case Left(es) => Left(e :: es)
          case Right(_) => Left(List(e))
        }
      }
      def functor[A, B](fa: Either[List[E], A])(f: A => B) = fa.map(f)
    }
    handle(eff)
  }

  def recover[R, A](eff: Eff[Effect with R, A])(f: E => Eff[R, A]): Eff[R, A] = {
    run[R, A](eff).flatMap {
      case Right(a) => Eff.Pure(a)
      case Left(e) => f(e)
    }
  }
}

object Exc {
  def apply[E](implicit exc: Exc[E]): exc.type = exc

  def raise[E](e: E)(implicit exc: Exc[E]): Eff[exc.Effect, Nothing] = exc.raise(e)

  def lift[E, A](either: Either[E, A])(implicit exc: Exc[E]): Eff[exc.Effect, A] = exc.lift(either)
}
