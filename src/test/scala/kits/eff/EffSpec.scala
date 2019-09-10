package kits.eff

import org.scalatest.FlatSpec

class EffSpec extends FlatSpec {
  "Eff" should "not stack overflow even if it counts up to 1,000,000 in State" in {
    implicit val stateInt = new State[Int] {}
    val N = 1000000
    def loop(implicit state: State[Int]): Eff[state.Effect, Unit] =
      for {
        s <- state.get
        _ <- if (s < N) {
          state.put(s + 1).flatMap(_ => loop(state))
        } else {
          Eff.Pure(s)
        }
      } yield ()
    assert(State[Int].run(0)(loop).run == (N, ()))
  }
}
