open System

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open Graphs

let username = "lambda-llama"

let handshake (p: Pipe.T): unit =
    let () = Pipe.write p (ProtocolData.Handshake {me=username})
    let (ProtocolData.HandshakeAck h) = Pipe.read p
    if h.you <> username
    then failwithf "Unexpected response: %A\n" h

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

let online host port strategy =
    let p = Pipe.connect host port
    handshake p
    let (ProtocolData.Setup setup) = Pipe.read p
    let initialState = Game.initialState setup
    Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=None; futures=[||]})
    let (finalState, scores) = play p setup.punter strategy initialState
    let scores =
        scores
        |> Array.sortBy (fun s -> s.punter)
        |> Array.map (fun s -> s.score)
        |> Array.toList
    let dists = Traversal.shortestPaths finalState.Graph
    let estimatedScores =
         [for p in 0..finalState.NumPlayers - 1
          -> Traversal.shortestPaths (Graph.subgraph finalState.Graph p)
             |> Game.score2 finalState dists]
    eprintf """{"scores": %A, "estimatedScores": %A, "me": %d}"""
        scores estimatedScores finalState.Me

let offline (strategy: Strategy.T) =
    let p = Pipe.std ()
    handshake p
    match Pipe.read p with
    | ProtocolData.Setup setup ->
      let chunk = (Game.initialState setup).Serialize ()
      Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=Some chunk; futures=[||]})
    | ProtocolData.RequestMove {move=move; state=Some chunk} ->
      let state = Game.applyMoves (Game.State.Deserialize chunk) move.moves
      let step = strategy.init state.Graph
      let (edge, newStrategyState) = step state
      let (u, v) = Edge.ends edge
      let vIndex = state.VIndex
      let (eu, ev) = (vIndex.e(u), vIndex.e(v))
      let nextMove = ProtocolData.Claim {punter=state.Me; source=eu; target=ev}
      let newState = { state with StrategyState = newStrategyState }
      Pipe.write p (ProtocolData.Move {move=nextMove; state=Some (newState.Serialize ())})
    | _ -> ()
    Pipe.close p

let runSimulation mapName =
    let map = System.IO.File.ReadAllText (sprintf "maps/%s.json" mapName)
    let competitors = [
        Strategy.bruteForce1
        Strategy.bruteForce3
        MinimaxStrategy.minimax
        // Strategy.randomEdge
    ]
    let (scores: int list) = Simulation.simulate (JsonConvert.DeserializeObject<JObject>(map) |> ProtocolData.deserializeMap) competitors
    List.zip competitors scores
    |> List.iter (fun (c, s) ->  printf "%s %d\n" c.name s)


[<EntryPoint>]
let main = function
| [|"--sim"; |] -> runSimulation "lambda"; 0
| [|"--sim"; map |] -> runSimulation map; 0
| [|port; strategyName|] when Map.containsKey strategyName Strategies.all ->
    online "punter.inf.ed.ac.uk" (int port) Strategies.all.[strategyName]; 0
| [||] -> offline Strategy.bruteForce1; 0
| _ ->
    Strategies.all |> Map.toSeq |> Seq.map fst
        |> String.concat "|"
        |> printf "usage:\n%%prog%% <--local|PORT> <%s>\n%%prog%% --server MAP\n --sim [MAP]"
    1
