import kotlinx.coroutines.*
import kotlin.system.measureTimeMillis

val numCpu = Runtime.getRuntime().availableProcessors()
val numNumbers = 1_000_000_000

suspend fun countNumbers(start: Int, endNum: Int): Int {
    var count = 0
    for (i in start..endNum) {
        count++
    }
    return count
}

fun CoroutineScope.spawnCounter(start: Int, endNum: Int): Deferred<Int> = async {
    countNumbers(start, endNum)
}

suspend fun collectResults(threads: List<Deferred<Int>>): List<Int> =
    threads.awaitAll()

fun main() = runBlocking {
    val perChunk = numNumbers / numCpu
    val remainder = numNumbers % numCpu

    val executionTime = measureTimeMillis {
        val threads = (0 until numCpu).map { i ->
            val start = i * perChunk + 1
            val endNum = start + perChunk - 1 + if (i == numCpu - 1) remainder else 0
            spawnCounter(start, endNum)
        }

        val results = collectResults(threads)
        val totalCount = results.sum()

        println("Count: $totalCount")
    }

    println("Execution Time: $executionTime milliseconds")
}
