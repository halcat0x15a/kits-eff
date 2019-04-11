package kits.eff

import org.scalatest.FlatSpec

class ExcSpec extends FlatSpec {
  "Exc" should "handle multiple failures" in {
    val e = for {
      _ <- Exc.fail(42)
      _ <- Exc.fail("hoge")
    } yield ()
    assert(Eff.run(Exc[Int].run(Exc[String].run(e))) == Left(List(42)))
    assert(Eff.run(Exc[String].run(Exc[Int].run(e))) == Right(Left(List(42))))
  }

  it should "collect failures" in {
    val e = Eff.map(
      Exc.fail("foo"),
      Exc.fail("bar"),
      Exc.fail("baz")
    )((_, _, _) => ())
    assert(Eff.run(Exc.run(e)) == Left(List("foo", "bar", "baz")))
  }

  it should "recover from the failure" in {
    val e = for {
      _ <- Exc.fail(new IllegalArgumentException("hoge"))
    } yield "fuga"
    assert(Eff.run(Exc.recover(e)(es => Eff.Pure(es.head.getMessage))) == "hoge")
  }
}
