module Strategy

open Graphs
open System
open System.Threading
open System.Threading.Tasks

type T = {
    name: string
    defaultState: Map<string, string>
    init: Graph.T -> Game.State -> Edge.T * Map<string, string>
}

let stateless name def step = { name = name; defaultState = def; init = fun _ -> step }
let withSetup name def setup step =
    {
        name = name
        defaultState = def
        init = fun initialGraph -> let data = setup initialGraph in fun game -> step data game
    }

let mixSlowFastTimeout name (timeoutMs: int) (slow: T) (fast: T) = {
    name = name
    defaultState = Map.empty
    init = fun initialGraph ->
        let s = slow.init initialGraph
        let f = fast.init initialGraph
        fun game ->
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            let (timeout, (edge, strategyState)) = 
                async {
                    let fastTask = async { return Some(f game) }
                    let tokenSource = new CancellationTokenSource()
                    let slowTask = async { return Some(Some(s { game with Graph = Graph.withCancelationToken game.Graph tokenSource.Token})) }
                    let timtoutTask = async {
                        do! Async.Sleep timeoutMs
                        return Some(None)
                    }
                    let timedTask = async {
                        let! c = Async.Choice [timtoutTask; slowTask]
                        return (Option.get c)
                    }
                    let! xs = [ timedTask; fastTask ] |> Async.Parallel
                    tokenSource.Cancel ()
                    return 
                        match xs.[0] with
                        | Some(r) -> (false, r)                    
                        | None -> (true, xs.[1] |> Option.get)
                } |> Async.RunSynchronously
            stopWatch.Stop()
            if timeout 
            then (edge, game.StrategyState)
            else (edge, strategyState)
}
