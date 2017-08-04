open System

let handshake (p: Pipe.T): Async<ProtocolData.SetupIn> = async {
    printf "Performing handshake... "
    let! _ = Pipe.write p "{\"me\":\"lambda-llama\"}"
    let! (ProtocolData.HandshakeAck h) = Pipe.read p
    if h.you <> "lambda-llama"
    then return (failwithf "Unexpected response: %A\n" h)
    else
        printf "OK!\n"
        let! (ProtocolData.Setup setup) = Pipe.read p
        let! _ = Pipe.write p (sprintf "{\"ready\":%d}" setup.punter)
        return setup
}

let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let rec go currState =
        async {
            let! message = Pipe.read p in
            match message with
            | ProtocolData.RequestMove moves ->
              let nextState = Game.applyMoveIn currState moves
              let (source, target) = strategy nextState
              let nextMove = (sprintf
                "{\"claim\":{\"punter\":%d,\"source\":%d,\"target\":%d}}"
                punter source target)
              let! () = Pipe.write p nextMove
              return! go nextState
            | ProtocolData.Stop stop -> return ()
            | _ -> failwithf "Unexpected response: %A\n" message
        }
    in go

let online port () = async {
    let! p = Pipe.connect port
    let! setup = handshake p
    let initialState = Game.initialState setup
    printf "Showtime with punter %d!\n" setup.punter
    let! () = play p setup.punter (Strategy.growFromMines) initialState
    return ()
}

[<EntryPoint>]
let main = function
| [|port|] ->
  Async.RunSynchronously (online (int port) ()); 0
| _ -> failwith "usage: %prog% PORT"
