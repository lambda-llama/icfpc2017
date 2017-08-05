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

let runSimulation mapName = 
    let map = System.IO.File.ReadAllText (sprintf "maps/%s.json" mapName)
    let competitors = [Strategy.bruteForceOneStep; Strategy.randomEdge]
    let (scores: int list) = Simulation.simulate (JsonConvert.DeserializeObject<JObject>(map) |> ProtocolData.deserializeMap) competitors
    printf "%A" scores

[<EntryPoint>]
let main = function
| [|"--sim"; |] -> runSimulation "lambda"; 0
| [|"--sim"; map |] -> runSimulation map; 0
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
        |> printf "usage:\n%%prog%% <--local|PORT> <%s>\n%%prog%% --server MAP\n --sim [MAP]"
    1
