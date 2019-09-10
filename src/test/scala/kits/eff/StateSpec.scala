package kits.eff

import org.scalatest.FlatSpec

class StateSpec extends FlatSpec {
  implicit val stateInt = new State[Int] {}

  "State" should "get and set the state" in {
    val e = for {
      s <- State.get[Int]
      _ <- State.put(s + 1)
    } yield s
    assert(State[Int].run(0)(e).run == (1, 0))
  }

  "transaction" should "protect the state" in {
    implicit val excString = new Exc[String] {}
    val e = for {
      _ <- State.modify((_: Int) + 1)
      _ <- Exc.raise("error")
    } yield ()
    assert(State[Int].run(0)(Exc[String].run(e)).run == (1, Left("error")))
    assert(State[Int].run(0)(Exc[String].run(State[Int].transaction(e))).run == (0, Left("error")))
  }
}
