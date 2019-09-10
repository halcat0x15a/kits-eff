package kits.eff

import org.scalatest.AsyncFlatSpec
import scala.concurrent.ExecutionContext

class TaskSpec extends AsyncFlatSpec {
  "Task" should "run as Future asynchronously" in {
    implicit val readerInt = new Reader[Int] {}
    implicit val writerInt = new Writer[Int] {}
    val e = for {
      n <- Reader.ask[Int]
      _ <- Writer.tell(n)
      m <- Task.async(n * n)
    } yield m
    Task.run(Reader[Int].run(2)(Writer[Int].run(e))).map { result =>
      assert(result == (Vector(2), 4))
    }
  }

  it should "run concurrently" in {
    val e = Eff.sequence(List(Task.async(Thread.sleep(1000)), Task.async(Thread.sleep(1000))))
    val s = System.nanoTime
    Task.run(e)(ExecutionContext.global).map { _ =>
      assert(System.nanoTime - s < 2 * 1000000000)
    }
  }
}
