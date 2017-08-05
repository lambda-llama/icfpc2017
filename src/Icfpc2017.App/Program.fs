open System

let username = "lambda-llama"

let handshake (p: Pipe.T): Async<ProtocolData.SetupIn> = async {
    let! _ = Pipe.write p (ProtocolData.Handshake {me=username})
    let! (ProtocolData.HandshakeAck h) = Pipe.read p
    if h.you <> username
    then return (failwithf "Unexpected response: %A\n" h)
    else
        let! (ProtocolData.Setup setup) = Pipe.read p
        let! _ = Pipe.write p (ProtocolData.Ready {ready=setup.punter})
        return setup
}

let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let rend = Game.Renderer.create "game"
    let rec go currState =
        async {
            let! message = Pipe.read p in
            match message with
            | ProtocolData.RequestMove moves ->
              let nextState = Game.applyMoveIn currState moves
              rend.dump nextState
              let (source, target) = strategy nextState
              let nextMove = ProtocolData.Claim {
                  punter=punter; 
                  source=int source; 
                  target=int target
              }
              let! () = Pipe.write p (ProtocolData.Move nextMove)
              return! go nextState
            | ProtocolData.Stop stop -> return ()
            | _ -> failwithf "Unexpected response: %A\n" message
        }
    in go

let online host port strategy = async {
    let! p = Pipe.connect host port
    let! setup = handshake p
    let initialState = Game.initialState setup
    let! _ = play p setup.punter strategy initialState
    printf "We: %d" (setup.punter)
    return ()
}

let clientStart host port strategyName =
    let strategy = Strategies.all.[strategyName] in
    online host port strategy |> Async.RunSynchronously

[<EntryPoint>]
let main = function
| [|"--server"; mapFilePath|] ->
    Server.start mapFilePath (7777); 0
| [|"--local"; strategyName|] when Map.containsKey strategyName Strategies.all ->
    clientStart "localhost" 7777 strategyName; 0
| [|port; strategyName|] when Map.containsKey strategyName Strategies.all ->
    clientStart "punter.inf.ed.ac.uk" (int port) strategyName; 0
| _ -> 
    Strategies.all |> Map.toSeq |> Seq.map fst
        |> String.concat "|"
        |> printf "usage:\n%%prog%% <--local|PORT> <%s>\n%%prog%% --server MAP"
    1
