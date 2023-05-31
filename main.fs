open System
open System.Diagnostics
open System.Threading.Tasks

let countNumbers start endNum =
    let mutable count = 0
    for i = start to endNum do
        count <- count + 1
    count

let spawnCounter start endNum =
    async {
        let! parent = Task.Factory.StartNew(fun () -> Async.CurrentAsync)
        let child = Task.Factory.StartNew(fun () -> countNumbers start endNum)
        let! _ = Async.Await(child)
        let result = child.Result
        return parent.Result, result
    }

let collectResults results =
    async {
        let! collectedResults = results |> Async.Parallel |> Async.RunSynchronously
        return List.sumBy snd collectedResults
    }

let main () =
    let numCpu = Environment.ProcessorCount
    let perChunk = 1000000000 / numCpu
    let remainder = 1000000000 % numCpu

    let stopwatch = Stopwatch.StartNew()

    let threads =
        [ for i in 0 .. numCpu - 1 ->
            spawnCounter (i * perChunk + 1) ((i + 1) * perChunk + if i = numCpu - 1 then remainder else 0) ]

    let totalCount = threads |> collectResults |> Async.RunSynchronously

    stopwatch.Stop()
    let executionTime = stopwatch.ElapsedMilliseconds

    printfn "Count: %d" totalCount
    printfn "Execution Time: %d milliseconds" executionTime

main()
