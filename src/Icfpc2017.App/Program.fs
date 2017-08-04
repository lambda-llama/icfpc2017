open System

[<EntryPoint>]
let main argv =
    let graph = Graph.create 3u [(0u, 1u); (0u, 2u )] in
    Graph.claimEdge graph 1u (0u, 1u) 
    |> Graph.toDot
    |> printfn "Graph:\n%s" 
    0
