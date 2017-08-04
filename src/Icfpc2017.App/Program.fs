open System

let handshake (p: Pipe.T) () = async {
    printf "Performing handshake... "
    let! _ = Pipe.write p "{\"me\":\"lambda-llama\"}" ()
    let! you = Pipe.read p ()
    if you <> "{\"you\":\"lambda-llama\"}" 
    then failwithf "Unexpected response: %s\n" you
    else          
        printf "OK!\n"
        let! map = Pipe.read p ()
        printf "<<< %s\n" map
        return ()
}

let online port () = async {
    let! p = Pipe.connect port ()
    let! _ = handshake p ()
    return ()
}

[<EntryPoint>]
let main = function
| [|port|] -> 
  Async.RunSynchronously (online (int port) ()); 0
| _ -> failwith "usage: %prog% PORT"