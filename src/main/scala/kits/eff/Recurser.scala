package kits.eff

import scala.annotation.tailrec

trait Recurser[F, R, A, B] {
  def pure(a: A): Eff[R, B]

  def tailRec[T]: PartialFunction[Fx[T], Either[T, Eff[R, B]]]

  def apply(eff: Eff[F with R, A]): Eff[R, B] = {
    @tailrec
    def loop(eff: Eff[F with R, A]): Eff[R, B] =
      eff match {
        case Eff.Pure(a) =>
          pure(a)
        case Eff.Impure(u, k) =>
          val f = tailRec[Any]
          if (f.isDefinedAt(u.value)) {
            f(u.value) match {
              case Left(a) =>
                loop(k(a))
              case Right(eff) =>
                eff
            }
          } else {
            Eff.Impure(new Union(u.value), Arrs((a: Any) => apply(k(a))))
          }
      }
    loop(eff)
  }
}

trait StateRecurser[F, R, S, A, B] {
  def pure(s: S, a: A): Eff[R, B]

  def tailRec[T](s: S): PartialFunction[Fx[T], Either[(S, T), Eff[R, B]]]

  def apply(s: S, eff: Eff[F with R, A])(implicit F: Manifest[F]): Eff[R, B] = {
    @tailrec
    def loop(s: S, eff: Eff[F with R, A]): Eff[R, B] =
      eff match {
        case Eff.Pure(a) =>
          pure(s, a)
        case Eff.Impure(u, k) =>
          val f = tailRec[Any](s)
          if (f.isDefinedAt(u.value)) {
            f(u.value) match {
              case Right(eff) =>
                eff
              case Left((s, a)) =>
                loop(s, k(a))
            }
          } else {
            Eff.Impure(new Union(u.value), Arrs((a: Any) => apply(s, k(a))))
          }
      }
    loop(s, eff)
  }
}
