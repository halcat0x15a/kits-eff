package kits.eff

import org.scalatest.FlatSpec

class ExcSpec extends FlatSpec {
  "Exc" should "handle multiple failures" in {
    val e = for {
      _ <- Exc.raise(42)
      _ <- Exc.raise("hoge")
    } yield ()
    assert(Eff.run(Exc[Int].run(Exc[String].run(e))) == Left(42))
    assert(Eff.run(Exc[String].run(Exc[Int].run(e))) == Right(Left(42)))
  }

  it should "collect failures" in {
    val e = Eff.sequence(List(Exc.raise("foo"), Exc.raise("bar"), Exc.raise("baz")))
    assert(Eff.run(Exc.accumulate(e)) == Left(List("foo", "bar", "baz")))
  }

  it should "recover from the failure" in {
    val e = for {
      _ <- Exc.raise(new IllegalArgumentException("hoge"))
    } yield "fuga"
    assert(Eff.run(Exc.recover(e)(e => Eff.Pure(e.getMessage))) == "hoge")
  }
}
