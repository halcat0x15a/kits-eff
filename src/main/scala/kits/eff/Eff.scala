package kits.eff

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Builder

trait Fx[A] extends Any

case class Union[-R, A](tag: Manifest[_], value: Fx[_]) {
  def decomp[F, S](implicit F: Manifest[F], ev: (F with S) <:< R): Either[Union[S, A], F with Fx[A]] =
    if (F.runtimeClass.isInstance(value) && tag <:< F)
      Right(value.asInstanceOf[F with Fx[A]])
    else
      Left(this.asInstanceOf[Union[S, A]])

  def extend[S]: Union[R with S, A] = this.asInstanceOf[Union[R with S, A]]
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
        Eff.Impure(u.extend[S], k :+ f)
    }

  def ap[S, B](f: Eff[S, A => B]): Eff[R with S, B] =
    this match {
      case Eff.Pure(v) =>
        f.map(_(v))
      case Eff.Impure(u, k) =>
        Eff.Impure(u.extend[S], k :+ f)
    }
}

object Eff {
  def apply[F, A](fa: F with Fx[A])(implicit F: Manifest[F]): Eff[F, A] = Impure(Union(F, fa), Arrs.id[F, A])

  def run[A](eff: Eff[Any, A]): A =
    (eff: @unchecked) match {
      case Pure(a) => a
    }

  def map[R0, R1, A0, A1, B](ra0: Eff[R0, A0], ra1: Eff[R1, A1])(f: (A0, A1) => B): Eff[R0 with R1, B] = ra0.ap(ra1.map(a1 => f(_, a1)))

  def map[R0, R1, R2, A0, A1, A2, B](ra0: Eff[R0, A0], ra1: Eff[R1, A1], ra2: Eff[R2, A2])(f: (A0, A1, A2) => B): Eff[R0 with R1 with R2, B] = ra0.ap(map(ra1, ra2)((a1, a2) => f(_, a1, a2)))

  def map[R0, R1, R2, R3, A0, A1, A2, A3, B](ra0: Eff[R0, A0], ra1: Eff[R1, A1], ra2: Eff[R2, A2], ra3: Eff[R3, A3])(f: (A0, A1, A2, A3) => B): Eff[R0 with R1 with R2 with R3, B] = ra0.ap(map(ra1, ra2, ra3)((a1, a2, a3) => f(_, a1, a2, a3)))

  def map[R0, R1, R2, R3, R4, A0, A1, A2, A3, A4, B](ra0: Eff[R0, A0], ra1: Eff[R1, A1], ra2: Eff[R2, A2], ra3: Eff[R3, A3], ra4: Eff[R4, A4])(f: (A0, A1, A2, A3, A4) => B): Eff[R0 with R1 with R2 with R3 with R4, B] = ra0.ap(map(ra1, ra2, ra3, ra4)((a1, a2, a3, a4) => f(_, a1, a2, a3, a4)))

  def traverse[R, A, B, M[X] <: TraversableOnce[X]](ma: M[A])(f: A => Eff[R, B])(implicit cbf: CanBuildFrom[M[A], B, M[B]]): Eff[R, M[B]] =
    ma.foldLeft(Pure(cbf()): Eff[R, Builder[B, M[B]]])((fmb, a) => map(fmb, f(a))((mb, b) => mb += b)).map(_.result)

  def sequence[R, A, M[X] <: TraversableOnce[X]](ma: M[Eff[R, A]])(implicit cbf: CanBuildFrom[M[Eff[R, A]], A, M[A]]): Eff[R, M[A]] =
    traverse(ma)(a => a)

  case class Pure[A](value: A) extends Eff[Any, A]

  case class Impure[-R, A, B](union: Union[R, A], arrs: Arrs[R, A, B]) extends Eff[R, B]
}
