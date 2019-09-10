package kits.eff

import org.scalatest.FlatSpec

class WriterSpec extends FlatSpec {
  implicit val writerString = new Writer[String] {}

  "Writer" should "tell output" in {
    val e = for {
      _ <- Writer.tell("hoge")
      _ <- Writer.tell("fuga")
    } yield ()
    assert(Writer[String].run(e).run == (Vector("hoge", "fuga"), ()))
  }

  it should "fold output" in {
    val e = for {
      _ <- Writer.tell("hoge")
      _ <- Writer.tell("fuga")
    } yield ()
    assert(Writer[String].fold(e)("")(_ + _).run == ("hogefuga", ()))
  }

  it should "listen output" in {
    val e = for {
      _ <- Writer.tell("hoge")
      _ <- Writer.tell("fuga")
    } yield ()
    assert(Writer[String].run(Writer[String].listen(e).map(_._1.size)).run == (Vector("hoge", "fuga"), 2))
  }

  it should "handle multiple output" in {
    implicit val writerInt = new Writer[Int] {}
    val e = for {
      _ <- Writer.tell(42)
      _ <- Writer.tell("hoge")
    } yield ()
    assert(Writer[String].run(Writer[Int].run(e)).run == (Vector("hoge"), (Vector(42), ())))
    assert(Writer[Int].run(Writer[String].run(e)).run == (Vector(42), (Vector("hoge"), ())))
  }
}
