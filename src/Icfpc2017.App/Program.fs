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

let online () = async {
    let! p = Pipe.connect 9018 ()
    let! _ = handshake p ()
    return ()
}

[<EntryPoint>]
let main argv =
    Async.RunSynchronously (online ()); 0
