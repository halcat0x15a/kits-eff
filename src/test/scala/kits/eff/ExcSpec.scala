package kits.eff

import org.scalatest.FlatSpec

class ExcSpec extends FlatSpec {
  implicit val excString = new Exc[String] {}

  "Exc" should "handle multiple failures" in {
    implicit val excInt = new Exc[Int] {}
    val e = for {
      _ <- Exc.raise(42)
      _ <- Exc.raise("hoge")
    } yield ()
    assert(Exc[Int].run(Exc[String].run(e)).run == Left(42))
    assert(Exc[String].run(Exc[Int].run(e)).run == Right(Left(42)))
  }

  it should "collect failures" in {
    val e = Eff.sequence(List(Exc.raise("foo"), Exc.raise("bar"), Exc.raise("baz")))
    assert(Exc[String].accumulate(e).run == Left(List("foo", "bar", "baz")))
  }

  it should "recover from the failure" in {
  implicit val excThrowable = new Exc[Throwable] {}
    val e = for {
      _ <- Exc.raise(new IllegalArgumentException("hoge"): Throwable)
    } yield "fuga"
    assert(Exc[Throwable].recover(e)((e: Throwable) => Eff.Pure(e.getMessage)).run == "hoge")
  }
}
