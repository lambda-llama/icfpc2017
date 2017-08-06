module Online

open System

open Graphs
open Pervasives

let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let sw = System.Diagnostics.Stopwatch.StartNew()
    let rend = Game.Renderer.create "game"
    sw.Stop()
    printfn "Game.Create: %dms" sw.ElapsedMilliseconds
    fun (initialState: Game.State) ->
        sw.Restart()
        let step = strategy.init initialState.Graph
        sw.Stop()
        printfn "Strategy.Init: %dms" sw.ElapsedMilliseconds
        let rec go serializedState =
            sw.Restart()
            let currState = Game.State.Deserialize(serializedState)
            sw.Stop()
            printfn "State.Deserialize: %dms" sw.ElapsedMilliseconds
            match Pipe.read p with
            | ProtocolData.RequestMove {move=move} ->
                sw.Restart()
                let nextState = Game.applyMoves currState move.moves
                sw.Stop()
                printfn "State.ApplyMoves: %dms" sw.ElapsedMilliseconds
                let vIndex = currState.VIndex
                sw.Restart()
                let (edge, newStrategyState) = step nextState
                sw.Stop()
                printfn "Strategy[%s].Step: %dms" strategy.name sw.ElapsedMilliseconds
                let (u, v) = Edge.ends edge
                let (eu, ev) = (vIndex.e(u), vIndex.e(v))
                let nextMove = ProtocolData.Claim {punter=punter; source=eu; target=ev}
                Pipe.write p (ProtocolData.Move {move=nextMove; state=None})
                let newState = { nextState with StrategyState = newStrategyState }
                sw.Restart()
                let blob = newState.Serialize()
                sw.Stop()
                printfn "State.Serialize: %dms" sw.ElapsedMilliseconds
                go blob
            | ProtocolData.Stop {stop=stop} ->
                (Game.applyMoves currState stop.moves, stop.scores)
            | message -> failwithf "Unexpected response: %A\n" message
        in go (initialState.Serialize())

let checkParam (key: string) (state: Map<string, string>): unit =
    if not (Map.containsKey key state) then
        failwithf "Unknown strategy parameter '%s', pick any of [%s]" key
            (String.Join(", ", state |> Map.toSeq |> Seq.map fst))

let rec addExtra (state: Map<string, string>) (extra: string list): Map<string, string> =
    match extra with
    | Prefix "--" option :: rest ->
        if option.Contains "=" then
            let [|key; value|] = option.Split('=', 2)
            checkParam key state
            addExtra (Map.add key value state) rest
        else
            checkParam option state
            addExtra (Map.add option (true.ToString()) state) rest
    | [] -> state
    | str :: _ -> failwithf "Unknown option '%s', did you mean '--%s'?" str str

let run host port (strategy : Strategy.T) (extra: string list) =
    let p = Pipe.connect host port
    Offline.handshake p
    let (ProtocolData.Setup setup) = Pipe.read p
    let initialState = Game.initialState setup (addExtra strategy.defaultState extra)
    Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=None; futures=[||]})
    let (finalState, scores) = play p setup.punter strategy initialState
    let scores =
        scores
        |> Array.sortBy (fun s -> s.punter)
        |> Array.map (fun s -> s.score)
    let dists = Traversal.shortestPaths finalState.Graph
    let estimatedScores =
         [|for p in 0..finalState.NumPlayers - 1
          -> Traversal.shortestPaths (Graph.subgraph finalState.Graph p)
             |> Game.score2 finalState dists|]
    let f (ss: int array) = String.Join (",", ss)
    eprintf """{"scores": [%s], "estimatedScores": [%s], "me": %d}"""
        (f scores) (f estimatedScores) finalState.Me
