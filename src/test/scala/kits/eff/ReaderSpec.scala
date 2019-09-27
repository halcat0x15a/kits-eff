package kits.eff

import org.scalatest.FlatSpec

class ReaderSpec extends FlatSpec {
  implicit val readerInt = new Reader[Int] {}

  "Reader" should "handle multiple input" in {
    implicit val readerString = new Reader[String] {}
    val e = for {
      i <- Reader.ask[Int]
      s <- Reader.ask[String]
    } yield s"$i$s"
    assert(Reader[Int].run(42)(Reader[String].run("hoge")(e)).run == "42hoge")
  }

  "local" should "modify the environment" in {
    val e = for {
      i <- Reader.ask[Int]
    } yield i / 2
    assert(Reader[Int].run(42)(Reader[Int].local(e)(_ * 2)).run == 42)
  }
}
