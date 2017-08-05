open System

open Newtonsoft.Json
open Newtonsoft.Json.Linq

let username = "lambda-llama"

let handshake (p: Pipe.T): unit = 
    let () = Pipe.write p (ProtocolData.Handshake {me=username})
    let (ProtocolData.HandshakeAck h) = Pipe.read p
    if h.you <> username
    then failwithf "Unexpected response: %A\n" h

let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let rend = Game.Renderer.create "game"
    fun (initialState: Game.State) ->
        let step = strategy.init initialState.Graph2
        let rec go currState =
            rend.dump currState
            match Pipe.read p with
            | ProtocolData.RequestMove moves ->
              let nextState = Game.applyMoveIn currState moves
              let (source, target) = Graphs.Graph.originalEnds currState.Graph2 (step nextState)
              let nextMove = ProtocolData.Claim {punter=punter; source=source; target=target}
              let () = Pipe.write p (ProtocolData.Move {move=nextMove; state=None})
              go nextState
            | ProtocolData.Stop stop ->
                    let sortedScores = stop.stop.scores |> Array.sortBy (fun x -> x.punter) 
                                                        |> Array.map (fun x -> x.score)
                    let sortedScoresAsStr = sortedScores |> Array.map string 
                                                         |> String.concat ","
                    eprintf """{"sortedScores": "%s" "me": "%d"}\n""" sortedScoresAsStr punter
            | message -> failwithf "Unexpected response: %A\n" message        
        in go initialState

let online host port strategy = 
    let p = Pipe.connect host port
    let () = handshake p
    let (ProtocolData.Setup setup) = Pipe.read p
    let initialState = Game.initialState setup    
    do Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=None})    
       play p setup.punter strategy initialState
       printf "We: %d" (setup.punter)

let offline (strategy: Strategy.T) = 
    let p = Pipe.std ()
    do handshake p
       match Pipe.read p with 
       | ProtocolData.Setup setup -> 
         let state = JsonConvert.SerializeObject (Game.initialState setup)
         Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=Some state})
       | ProtocolData.RequestMove ({state=Some state} as moveIn) ->
         let currState = JsonConvert.DeserializeObject state :?> Game.State
         let nextState = Game.applyMoveIn currState moveIn
         let (source, target) = strategy.init (currState.Graph2 (* TODO: precompute me*)) nextState |> Graphs.Graph.originalEnds currState.Graph2
         let nextMove = ProtocolData.Claim {punter=currState.Me; source=source; target=target}
         Pipe.write p (ProtocolData.Move {move=nextMove; state=Some (JsonConvert.SerializeObject nextState)})          
       | _ -> ()      
       Pipe.close p

let runSimulation mapName = 
    let map = System.IO.File.ReadAllText (sprintf "maps/%s.json" mapName)
    let competitors = [
        Strategy.bruteForce1
        // Strategy.bruteForce3
        Strategy.growFromMines
        Strategy.randomEdge
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
