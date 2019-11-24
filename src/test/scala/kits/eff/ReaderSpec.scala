package kits.eff

import org.scalatest.FlatSpec

class ReaderSpec extends FlatSpec {
  "Reader" should "handle multiple input" in {
    val e = for {
      i <- Reader.ask[Int]
      s <- Reader.ask[String]
    } yield s"$i$s"
    assert(Reader.run(42)(Reader.run("hoge")(e)).run == "42hoge")
  }

  it should "consider subtypes" in {
    val e = for {
      cs <- Reader.ask[CharSequence]
      s <- Reader.ask[String]
    } yield s"$s$cs"
    assert(Reader.run("fuga": CharSequence)(Reader.run("hoge")(e)).run == "hogefuga")
  }

  "local" should "modify the environment" in {
    val e = for {
      i <- Reader.ask[Int]
    } yield i / 2
    assert(Reader.run(42)(Reader.local((_: Int) * 2)(e)).run == 42)
  }
}
