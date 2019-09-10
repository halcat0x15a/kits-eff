package kits.eff

trait Interpreter[F, R, A, B] {
  def pure(a: A): Eff[R, B]

  def flatMap[T](f: T => Eff[R, B]): PartialFunction[Fx[T], Eff[R, B]]

  def apply(eff: Eff[F with R, A]): Eff[R, B] =
    eff match {
      case Eff.Pure(a) =>
        pure(a)
      case Eff.Impure(u, k) =>
        val f = flatMap((a: Any) => apply(k(a)))
        if (f.isDefinedAt(u.value))
          f(u.value)
        else
          Eff.Impure(new Union(u.value), Arrs((a: Any) => apply(k(a))))
    }
}

trait ApplicativeInterpreter[F, R] {
  type Result[A]

  def functor[A, B](ra: Result[A])(f: A => B): Result[B]

  def pure[A](a: A): Eff[R, Result[A]]

  def flatMap[A, B](f: A => Eff[R, Result[B]]): PartialFunction[Fx[A], Eff[R, Result[B]]]

  def ap[A, B](f: Eff[R, Result[A => B]]): PartialFunction[Fx[A], Eff[R, Result[B]]]

  def apply[A](eff: Eff[F with R, A]): Eff[R, Result[A]] =
    eff match {
      case Eff.Pure(a) =>
        pure(a)
      case Eff.Impure(u, k) =>
        k match {
          case Arrs.LeafA(k) =>
            val f = ap[Any, A](apply(k))
            if (f.isDefinedAt(u.value))
              f(u.value)
            else
              Eff.Impure(new Union(u.value), Arrs(apply(k).map(r => (a: Any) => functor(r)(_(a)): Result[A])))
          case _ =>
            val f = flatMap[Any, A](a => apply(k(a)))
            if (f.isDefinedAt(u.value))
              f(u.value)
            else
              Eff.Impure(new Union(u.value), Arrs((a: Any) => apply[A](k(a))))
        }
    }
}
