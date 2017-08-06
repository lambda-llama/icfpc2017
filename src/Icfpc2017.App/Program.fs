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
            | ProtocolData.RequestMove moves ->
              let nextState = Game.applyMoveIn currState moves
              let vIndex = currState.VIndex
              let (u, v) = step nextState |> Edge.ends
              let (eu, ev) = (vIndex.e(u), vIndex.e(v))
              let nextMove = ProtocolData.Claim {punter=punter; source=eu; target=ev}
              let () = Pipe.write p (ProtocolData.Move {move=nextMove; state=None})
              go nextState
            | ProtocolData.Stop stop ->
                let sortedScores =
                    stop.stop.scores
                    |> Array.sortBy (fun x -> x.punter)
                    |> Array.map (fun x -> x.score)
                    |> Array.toList
                eprintf """{"sortedScores": "%A" "me": "%d"}\n""" sortedScores punter
                currState
            | message -> failwithf "Unexpected response: %A\n" message
        in go initialState

let online host port strategy =
    let p = Pipe.connect host port
    let () = handshake p
    let (ProtocolData.Setup setup) = Pipe.read p
    let initialState = Game.initialState setup
    do Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=None; futures=[||]})
       let finalState = play p setup.punter strategy initialState
       let dists = Traversal.shortestPaths finalState.Graph
       let finalScores =
            [0..finalState.NumPlayers - 1]
            |> List.map (fun p -> Traversal.shortestPaths (Graph.subgraph finalState.Graph p))
            |> List.map (Game.score2 finalState dists)
       printfn "We: %d, scores: [%s]" (setup.punter) (String.Join("; ", finalScores))

let offline (strategy: Strategy.T) =
    let p = Pipe.std ()
    do handshake p
       match Pipe.read p with
       | ProtocolData.Setup setup ->
         let state = (Game.initialState setup).Serialize ()
         Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=Some state; futures=[||]})
       | ProtocolData.RequestMove ({state=Some state} as moveIn) ->
         let currState = Game.State.Deserialize state
         let nextState = Game.applyMoveIn currState moveIn
         let (source, target) = strategy.init currState.Graph nextState |> Edge.ends
         (* let nextMove = ProtocolData.Claim {punter=currState.Me; source=source; target=target}
         Pipe.write p (ProtocolData.Move {move=nextMove; state=Some (JsonConvert.SerializeObject nextState)})           *)
         ()
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
