package kits.eff

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed abstract class Task extends Product with Serializable

object Task {
  def context: Eff[Task, ExecutionContext] = Eff(Context)

  def lift[A](f: ExecutionContext => Future[A]): Eff[Task, A] = Eff(Lift(f))

  def async[A](a: => A): Eff[Task, A] = lift(Future(a)(_))

  def run[A](eff: Eff[Task, A])(implicit ec: ExecutionContext): Future[A] = {
    val handle = new ApplicativeInterpreter[Task, Any] {
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

  case object Context extends Task with Fx[ExecutionContext]

  case class Lift[A](future: ExecutionContext => Future[A]) extends Task with Fx[A]
}
