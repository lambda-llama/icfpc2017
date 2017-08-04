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

let rec play g n = 
    if n = 0
        then [g]
        else 
            let (u, v) = Strategy.growFromMines g in
            let g1 = Game.applyMoveIn g { move = { moves = [| ProtocolData.Claim { punter = 0; source = int u; target = int v }|] }; } in
            g :: play g1 (n - 1)

[<EntryPoint>]
let main argv =
    // Async.RunSynchronously (online ()); 0
    let game =  """{"punter":0, "punters":2, "map":{"sites":[{"id":4},{"id":1},{"id":3},{"id":6},{"id":5},{"id":0},{"id":7},{"id":2}], "rivers":[{"source":3,"target":4},{"source":0,"target":1},{"source":2,"target":3}, {"source":1,"target":3},{"source":5,"target":6},{"source":4,"target":5}, {"source":3,"target":5},{"source":6,"target":7},{"source":5,"target":7}, {"source":1,"target":7},{"source":0,"target":7},{"source":1,"target":2}], "mines":[1,5]}}""" in
    let setup = match ProtocolData.deserialize game with
        | ProtocolData.MessageIn.Setup setup -> setup
        | _ -> failwith ":("
    in
    let game = Game.initialState setup in
    let games = play game 3 in
    List.map (fun (g: Game.State) -> sprintf "%s" (Graph.toDot g.Graph)) games
    |> String.concat "\n\n"
    |> printf "%s"
    0
