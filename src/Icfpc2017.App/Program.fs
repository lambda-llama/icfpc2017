open System

[<EntryPoint>]
let main args =
    let arglist = args |> Array.toList
    match arglist with
    | ["--sim"; mapPath] -> Simulation.run mapPath; 0
    | port :: strategyName :: extra when Map.containsKey strategyName Strategies.all ->
        Online.run "punter.inf.ed.ac.uk" (int port) Strategies.all.[strategyName] extra; 0
    | [] -> Offline.run BruteForceStrategy.bruteForce1; 0
    | _ ->
        Strategies.all |> Map.toSeq |> Seq.map fst
            |> String.concat "|"
            |> printf "usage:\n%%prog%% <--local|PORT> <%s>\n%%prog%% --server MAP\n --sim [MAP]"
        1
