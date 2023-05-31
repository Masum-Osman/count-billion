import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

object Main extends App {

  val NUM_CPU: Int = Runtime.getRuntime.availableProcessors
  val NUM_NUMBERS: Int = 1000000000

  def countNumbers(start: Int, end: Int): Int = {
    var count = 0
    for (i <- start to end) {
      count += 1
    }
    count
  }

  def spawnCounter(start: Int, end: Int): Future[Int] = Future {
    countNumbers(start, end)
  }

  val perChunk: Int = NUM_NUMBERS / NUM_CPU
  val remainder: Int = NUM_NUMBERS % NUM_CPU

  val startTime: Long = System.currentTimeMillis()

  val threads: Seq[Future[Int]] = for (i <- 0 until NUM_CPU) yield {
    val s = i * perChunk + 1
    val e = if (i == NUM_CPU - 1) s + perChunk + remainder - 1 else s + perChunk - 1
    spawnCounter(s, e)
  }

  val totalCount: Int = Await.result(Future.sequence(threads), Duration.Inf).sum

  val endTime: Long = System.currentTimeMillis()
  val executionTime: Long = endTime - startTime

  println(s"Count: $totalCount")
  println(s"Execution Time: $executionTime milliseconds")
}
