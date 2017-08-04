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

let online port strategy = async {
    let! p = Pipe.connect port
    let! setup = handshake p
    let initialState = Game.initialState setup
    return! play p setup.punter strategy initialState
}

[<EntryPoint>]
let main = function
| [|port; strategyName|] when Map.containsKey strategyName Strategy.all ->
  let strategy = Strategy.all.[strategyName] in
  Async.RunSynchronously (online (int port) strategy); 0
| _ -> 
  Strategy.all |> Map.toSeq |> Seq.map fst 
    |> String.concat "|"
    |> failwithf "usage: %%prog%% PORT %s"
