import java.util.concurrent.*

def numCpu = Runtime.runtime.availableProcessors()
def numNumbers = 1000000000

def countNumbers(start, endNum) {
    def count = 0
    for (def i = start; i <= endNum; i++) {
        count++
    }
    return count
}

def spawnCounter(start, endNum) {
    Executors.newSingleThreadExecutor().submit({
        countNumbers(start, endNum)
    } as Callable<Integer>)
}

def collectResults(threads) {
    threads.collect {
        it.get()
    }
}

def main() {
    def perChunk = numNumbers / numCpu
    def remainder = numNumbers % numCpu

    def startTime = System.currentTimeMillis()

    def threads = (0..<numCpu).collect { i ->
        def start = i * perChunk + 1
        def endNum = i == numCpu - 1 ? start + perChunk + remainder - 1 : start + perChunk - 1
        spawnCounter(start, endNum)
    }

    def results = collectResults(threads)
    def totalCount = results.sum()

    def endTime = System.currentTimeMillis()
    def executionTime = endTime - startTime

    println "Count: $totalCount"
    println "Execution Time: $executionTime milliseconds"
}

main()
