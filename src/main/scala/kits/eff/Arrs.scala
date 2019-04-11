package kits.eff

import scala.annotation.tailrec

sealed abstract class Arrs[-R, A, B] extends Product with Serializable {
  def :+[S, C](f: Eff[S, B => C]): Arrs[R with S, A, C] =
    this match {
      case Arrs.LeafA(v) => Arrs.LeafA(v.ap(f.map(x => (_: A => B).andThen(x))))
      case _ => Arrs.Node(this, Arrs.LeafM((b: B) => f.map(_(b))))
    }

  def :+[S, C](f: B => Eff[S, C]): Arrs[R with S, A, C] = Arrs.Node(this, Arrs.LeafM(f))

  def ++[S, C](that: Arrs[S, B, C]): Arrs[R with S, A, C] =
    that match {
      case Arrs.LeafA(v) => this :+ v
      case _ => Arrs.Node(this, that)
    }

  def apply(value: A): Eff[R, B] = {
    @tailrec
    def loop[A](value: A, arrs: Arrs[R, A, B]): Eff[R, B] =
      arrs match {
        case Arrs.LeafA(f) => f.map(_(value))
        case Arrs.LeafM(f) => f(value)
        case Arrs.Node(Arrs.LeafA(f), r) =>
          f.map(_(value)) match {
            case Eff.Pure(v) => loop(v, r)
            case Eff.Impure(u, k) =>
              Eff.Impure(u, k ++ r)
          }
        case Arrs.Node(Arrs.LeafM(f), r) =>
          f(value) match {
            case Eff.Pure(v) => loop(v, r)
            case Eff.Impure(u, k) =>
              Eff.Impure(u, k ++ r)
          }
        case Arrs.Node(Arrs.Node(ll, lr), r) =>
          loop(value, Arrs.Node(ll, Arrs.Node(lr, r)))
      }
    loop(value, this)
  }
}

object Arrs {
  def id[R, A]: Arrs[R, A, A] = Arrs.LeafA(Eff.Pure(a => a))

  def apply[R, A, B](f: Eff[R, A => B]): Arrs[R, A, B] = Arrs.LeafA(f)

  def apply[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] = Arrs.LeafM(f)

  case class LeafA[-R, A, B](value: Eff[R, A => B]) extends Arrs[R, A, B]

  case class LeafM[-R, A, B](value: A => Eff[R, B]) extends Arrs[R, A, B]

  case class Node[-R, A, B, C](left: Arrs[R, A, B], right: Arrs[R, B, C]) extends Arrs[R, A, C]
}
