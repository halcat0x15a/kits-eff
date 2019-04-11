package kits.eff

sealed abstract class Opt extends Product with Serializable

object Opt {
  def empty: Eff[Opt, Nothing] = Eff(Empty)

  def lift[A](option: Option[A]): Eff[Opt, A] =
    option match {
      case None => empty
      case Some(a) => Eff.Pure(a)
    }

  def run[R, A](eff: Eff[Opt with R, A]): Eff[R, Option[A]] = {
    val handle = new Interpreter[Opt, R] {
      type Result[A] = Option[A]
      def pure[A](a: A) = Eff.Pure(Some(a))
      def flatMap[A, B](fa: Opt with Fx[A])(k: A => Eff[R, Option[B]]) =
        fa match {
          case Empty => Eff.Pure(None)
        }
      def map[A, B](fa: Option[A])(f: A => B) = fa.map(f)
    }
    handle(eff)
  }

  case object Empty extends Opt with Fx[Nothing]
}
