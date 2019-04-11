package kits.eff

trait Interpreter[F, R] {
  type Result[A]

  def pure[A](a: A): Eff[R, Result[A]]

  def flatMap[A, B](fa: F with Fx[A])(f: A => Eff[R, Result[B]]): Eff[R, Result[B]]

  def map[A, B](ra: Result[A])(f: A => B): Result[B]

  def ap[A, B](fa: F with Fx[A])(f: Eff[R, Result[A => B]]): Eff[R, Result[B]] = flatMap(fa)(a => f.map(r => map(r)(_(a))))

  final def apply[A](eff: Eff[F with R, A])(implicit F: Manifest[F]): Eff[R, Result[A]] =
    eff match {
      case Eff.Pure(a) =>
        pure(a)
      case Eff.Impure(u, Arrs.LeafA(k)) =>
        u.decomp[F, R] match {
          case Right(fa) =>
            ap(fa)(apply(k))
          case Left(r) =>
            Eff.Impure(r, Arrs(apply(k).map(r => (a: Any) => map(r)(_(a)): Result[A])))
        }
      case Eff.Impure(u, k) =>
        u.decomp[F, R] match {
          case Right(fa) =>
            flatMap(fa)(a => apply(k(a)))
          case Left(r) =>
            Eff.Impure(r, Arrs((a: Any) => apply[A](k(a))))
        }
    }
}
