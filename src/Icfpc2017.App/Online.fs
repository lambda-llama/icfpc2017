module Online

open System

open Graphs

let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let rend = Game.Renderer.create "game"
    fun (initialState: Game.State) ->
        let step = strategy.init initialState.Graph
        let rec go currState =
            rend.dump currState
            match Pipe.read p with
            | ProtocolData.RequestMove {move=move} ->
              let nextState = Game.applyMoves currState move.moves
              let vIndex = currState.VIndex
              let (edge, newStrategyState) = step nextState
              let (u, v) = Edge.ends edge
              let (eu, ev) = (vIndex.e(u), vIndex.e(v))
              let nextMove = ProtocolData.Claim {punter=punter; source=eu; target=ev}
              Pipe.write p (ProtocolData.Move {move=nextMove; state=None})
              go { nextState with StrategyState = newStrategyState }
            | ProtocolData.Stop {stop=stop} ->
              (Game.applyMoves currState stop.moves, stop.scores)
            | message -> failwithf "Unexpected response: %A\n" message
        in go initialState

let run host port strategy =
    let p = Pipe.connect host port
    Offline.handshake p
    let (ProtocolData.Setup setup) = Pipe.read p
    let initialState = Game.initialState setup
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
