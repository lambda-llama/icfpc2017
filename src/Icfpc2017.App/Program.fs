open System

let handshake (p: Pipe.T) (): Async<ProtocolData.SetupIn> = async {
    printf "Performing handshake... "
    let! _ = Pipe.write p "{\"me\":\"lambda-llama\"}" ()
    let! (ProtocolData.HandshakeAck h) = Pipe.read p ()
    if h.you <> "lambda-llama"
    then return (failwithf "Unexpected response: %A\n" h)
    else          
        printf "OK!\n"
        let! (ProtocolData.Setup map) = Pipe.read p ()
        return map
}

let online port () = async {
    let! p = Pipe.connect port ()
    let! setup = handshake p ()
    printf "Setup: %A\n" setup
    return ()
}

[<EntryPoint>]
let main = function
| [|port|] -> 
  Async.RunSynchronously (online (int port) ()); 0
| _ -> failwith "usage: %prog% PORT"