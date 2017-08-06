module Online

open System

open Graphs
open Pervasives

let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let rend =
        time "Game.Create" (fun () -> Game.Renderer.create "game")
    fun (initialState: Game.State) ->
        let step =
            time "Strategy.Init" (fun () -> strategy.init initialState.Graph)
        let rec go serializedState =
            let currState =
                time "State.Deserialize" (fun () -> Game.State.Deserialize(serializedState))
            match Pipe.read p with
            | ProtocolData.RequestMove {move=move} ->
                let nextState =
                    time "State.ApplyMoves" (fun () -> Game.applyMoves currState move.moves)
                let vIndex = currState.VIndex
                let (edge, newStrategyState) =
                    time (sprintf "Strategy[%s].Step" strategy.name) (fun () -> step nextState)
                let (u, v) = Edge.ends edge
                let (eu, ev) = (vIndex.e(u), vIndex.e(v))
                let nextMove = ProtocolData.Claim {punter=punter; source=eu; target=ev}
                Pipe.write p (ProtocolData.Move {move=nextMove; state=None})
                let newState = { nextState with StrategyState = newStrategyState }
                let blob = time "State.Serialize" newState.Serialize
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
