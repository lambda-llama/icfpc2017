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
    let rec go currState =
        rend.dump currState
        match Pipe.read p with
        | ProtocolData.RequestMove moves ->
          let nextState = Game.applyMoveIn currState moves
          let (source, target) = strategy nextState
          let nextMove = ProtocolData.Claim {punter=punter; source=source; target=target}
          let () = Pipe.write p (ProtocolData.Move {move=nextMove; state=None})
          go nextState
        | ProtocolData.Stop stop -> ()
        | message -> failwithf "Unexpected response: %A\n" message        
    in go

let online host port strategy = 
    let p = Pipe.connect host port
    let () = handshake p
    let (ProtocolData.Setup setup) = Pipe.read p
    let initialState = Game.initialState setup    
    do Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=None})    
       play p setup.punter strategy initialState
       printf "We: %d" (setup.punter)

let offline strategy = 
    let p = Pipe.std ()
    do handshake p
       match Pipe.read p with 
       | ProtocolData.Setup setup -> 
         let state = JsonConvert.SerializeObject (Game.initialState setup)
         Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=Some state})
       | ProtocolData.RequestMove ({state=Some state} as moveIn) ->
         let currState = JsonConvert.DeserializeObject state :?> Game.State
         let nextState = Game.applyMoveIn currState moveIn
         let (source, target) = strategy nextState
         let nextMove = ProtocolData.Claim {punter=currState.Me; source=source; target=target}
         Pipe.write p (ProtocolData.Move {move=nextMove; state=Some (JsonConvert.SerializeObject nextState)})          
       | _ -> ()      
       Pipe.close p

let clientStart host port strategyName =
    online host port Strategies.all.[strategyName]

let runSimulation () = 
    let map = """{"sites":[{"id":4},{"id":1},{"id":3},{"id":6},{"id":5},{"id":0},{"id":7},{"id":2}],
                  "rivers":[{"source":3,"target":4},{"source":0,"target":1},{"source":2,"target":3},
                            {"source":1,"target":3},{"source":5,"target":6},{"source":4,"target":5},
                            {"source":3,"target":5},{"source":6,"target":7},{"source":5,"target":7},
                            {"source":1,"target":7},{"source":0,"target":7},{"source":1,"target":2}],
                  "mines":[1,5]}"""
    let competitors = [Strategy.bruteForceOneStep; Strategy.randomEdge]
    let (scores: int list) = Simulation.simulate (JsonConvert.DeserializeObject<JObject>(map) |> ProtocolData.deserializeMap) competitors
    printf "%A" scores
    

[<EntryPoint>]
let main = function
| [|"--sim"|] -> runSimulation (); 0
| [|"--server"; mapFilePath|] ->
    (* Server.start mapFilePath (7777); *) 0
| [|"--local"; strategyName|] when Map.containsKey strategyName Strategies.all ->
    clientStart "localhost" 7777 strategyName; 0
| [|port; strategyName|] when Map.containsKey strategyName Strategies.all ->
    clientStart "punter.inf.ed.ac.uk" (int port) strategyName; 0
| [||] ->
    offline Strategy.bruteForceOneStep; 0
| _ -> 
    Strategies.all |> Map.toSeq |> Seq.map fst
        |> String.concat "|"
        |> printf "usage:\n%%prog%% <--local|PORT> <%s>\n%%prog%% --server MAP\n --sim"
    1
