package kits.eff

import scala.collection.compat._
import scala.collection.mutable.Builder

trait Fx[A] extends Any

class Union[-R, A](val value: Fx[A]) extends AnyVal {
  def widen[S]: Union[R with S, A] = new Union[R with S, A](value)
}

sealed abstract class Eff[-R, +A] extends Product with Serializable {
  def map[B](f: A => B): Eff[R, B] =
    this match {
      case Eff.Pure(v) =>
        Eff.Pure(f(v))
      case Eff.Impure(u, k) =>
        Eff.Impure(u, k :+ Eff.Pure(f))
    }

  def flatMap[S, B](f: A => Eff[S, B]): Eff[R with S, B] =
    this match {
      case Eff.Pure(v) =>
        f(v)
      case Eff.Impure(u, k) =>
        Eff.Impure(u.widen[S], k :+ f)
    }

  def ap[S, B](f: Eff[S, A => B]): Eff[R with S, B] =
    this match {
      case Eff.Pure(v) =>
        f.map(_(v))
      case Eff.Impure(u, k) =>
        Eff.Impure(u.widen[S], k :+ f)
    }

  def flatten[S, B](implicit ev: A <:< Eff[S, B]): Eff[R with S, B] =
    flatMap(ev)

  def zipWith[S, B, C](that: Eff[S, B])(f: (A, B) => C): Eff[R with S, C] =
    ap(that.map(b => f(_, b)))

  def zip[S, B](that: Eff[S, B]): Eff[R with S, (A, B)] =
    zipWith(that)((_, _))

  def run(implicit ev: Any <:< R): A =
    (this: @unchecked) match {
      case Eff.Pure(a) => a
    }
}

object Eff {
  def apply[F, A](fa: F with Fx[A]): Eff[F, A] = Impure(new Union(fa), Arrs.id[F, A])

  def run[A](eff: Eff[Any, A]): A = eff.run

  def traverse[R, A, B, M[X] <: IterableOnce[X]](ma: M[A])(f: A => Eff[R, B])(implicit cbf: Factory[B, M[B]]): Eff[R, M[B]] =
    ma.iterator.foldLeft(Pure(cbf.newBuilder): Eff[R, Builder[B, M[B]]])((fmb, a) => fmb.zipWith(f(a))((mb, b) => mb += b)).map(_.result)

  def sequence[R, A, M[X] <: IterableOnce[X]](ma: M[Eff[R, A]])(implicit cbf: Factory[A, M[A]]): Eff[R, M[A]] =
    traverse(ma)(a => a)

  case class Pure[A](value: A) extends Eff[Any, A]

  case class Impure[-R, A, B](union: Union[R, A], arrs: Arrs[R, A, B]) extends Eff[R, B]
}
