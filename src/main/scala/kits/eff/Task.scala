package kits.eff

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object Task {
  sealed abstract class Effect extends Product with Serializable

  case object Context extends Effect with Fx[ExecutionContext]

  case class Lift[A](future: ExecutionContext => Future[A]) extends Effect with Fx[A]

  def context: Eff[Effect, ExecutionContext] = Eff(Context)

  def lift[A](f: ExecutionContext => Future[A]): Eff[Effect, A] = Eff(Lift(f))

  def async[A](a: => A): Eff[Effect, A] = lift(Future(a)(_))

  def run[A](eff: Eff[Effect, A])(implicit ec: ExecutionContext): Future[A] = {
    val handle = new ApplicativeInterpreter[Effect, Any] {
      type Result[A] = Future[A]
      def pure[A](a: A) = Eff.Pure(Future.successful(a))
      def flatMap[A, B](k: A => Eff[Any, Future[B]]) = {
        case Context => k(ec)
        case Lift(f) => Eff.Pure(f(ec).flatMap(a => Eff.run(k(a))))
      }
      def ap[A, B](k: Eff[Any, Future[A => B]]) = {
        case Context => k.map(_.map(_(ec)))
        case Lift(f) => Eff.Pure(f(ec).flatMap(a => Eff.run(k).map(_(a))))
      }
      def functor[A, B](fa: Future[A])(f: A => B) = fa.map(f)
    }
    Eff.run(handle(eff))
  }
}
