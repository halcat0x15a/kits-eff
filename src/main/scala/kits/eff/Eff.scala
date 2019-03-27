package kits.eff

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

trait Fx[A] extends Any

case class Union[R, A](tag: Manifest[_], value: Fx[_]) {
  def decomp[F, S](implicit F: Manifest[F]): Either[Union[S, A], F with Fx[A]] =
    if (F.runtimeClass.isInstance(value) && tag <:< F)
      Right(value.asInstanceOf[F with Fx[A]])
    else
      Left(this.asInstanceOf[Union[S, A]])

  def extend[S]: Union[R with S, A] = this.asInstanceOf[Union[R with S, A]]
}

sealed abstract class Eff[-R, +A] extends Product with Serializable {
  def map[B](f: A => B): Eff[R, B]

  def flatMap[S, B](f: A => Eff[S, B]): Eff[R with S, B]
}

object Eff {
  def apply[F, A](fa: F with Fx[A])(implicit F: Manifest[F]): Eff[F, A] = Impure(Union(F, fa), Arrs.Leaf((a: A) => Pure(a)))

  def run[A](eff: Eff[Any, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
    }

  def join[R, S, A](eff: Eff[R, Eff[S, A]]): Eff[R with S, A] = eff.flatMap(x => x)

  def traverse[R, A, B, M[X] <: TraversableOnce[X]](in: M[A])(f: A => Eff[R, B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Eff[R, M[B]] =
    in.foldLeft(Eff.Pure(cbf(in)): Eff[R, Builder[B, M[B]]]) { (eff, a) =>
      for {
        builder <- eff
        b <- f(a)
      } yield builder += b
    }.map(_.result())

  def sequence[R, A, M[X] <: TraversableOnce[X]](in: M[Eff[R, A]])(implicit cbf: CanBuildFrom[M[Eff[R, A]], A, M[A]]): Eff[R, M[A]] = traverse(in)(x => x)

  case class Pure[A](value: A) extends Eff[Any, A] {
    def map[B](f: A => B): Eff[Any, B] = Pure(f(value))
    def flatMap[S, B](f: A => Eff[S, B]): Eff[S, B] = f(value)
  }

  case class Impure[R, A, B](union: Union[R, A], arrs: Arrs[R, A, B]) extends Eff[R, B] {
    def map[C](f: B => C): Eff[R, C] = Impure(union, arrs :+ (x => Pure(f(x))))
    def flatMap[S, C](f: B => Eff[S, C]): Eff[R with S, C] = Impure(union.extend[S], arrs :+ f)
  }
}
