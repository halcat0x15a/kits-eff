package kits.eff

import org.scalatest.AsyncFlatSpec
import scala.concurrent.ExecutionContext

class TaskSpec extends AsyncFlatSpec {
  "Task" should "run as Future asynchronously" in {
    val e = for {
      n <- Reader.ask[Int]
      _ <- Writer.tell(n)
      m <- Task.async(n * n)
    } yield m
    Task.run(Reader.run(2)(Writer.run(e))).map { result =>
      assert(result == (Vector(2), 4))
    }
  }

  it should "run concurrently" in {
    val e = Eff.map(Task.async(Thread.sleep(1000)), Task.async(Thread.sleep(1000)))((_, _) => ())
    val s = System.nanoTime
    Task.run(e)(ExecutionContext.global).map { _ =>
      assert(System.nanoTime - s < 2 * 1000000000)
    }
  }
}
