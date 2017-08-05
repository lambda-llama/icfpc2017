module GreadyStrategy

let greadyStrategy: Strategy.T =
    Strategy.stateless "gready" (fun game ->
        let graph = game.Graph
        let getScore (edge:Graph.Edge) =
                match edge.Color with
                | Some _ -> 0
                | None -> let v1,v2 = edge.Ends in game.Union.GetIncrement(v1, v2, game.BFSDist)
        let goodEdge = graph.Edges |> List.maxBy getScore
        let ver1, ver2 = goodEdge.Ends
        game.Union.Unite ver1 ver2
        Graph.fromOriginalEnds game.Graph (ver1, ver2)
    )