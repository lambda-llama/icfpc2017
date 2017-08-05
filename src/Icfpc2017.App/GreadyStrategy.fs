module GreadyStrategy

let greadyStrategy: Strategy.T = fun game ->
    let graph = game.Graph
    let me = game.Me
    let getScore (edge:Graph.Edge) = 
            match edge.Color with
            | Some c -> 0
            | None -> let v1,v2 = edge.Ends in game.Union.GetIncrement(v1, v2, game.BFSDist)
    let goodEdge = graph.Edges |> List.maxBy getScore
    let ver1, ver2 = goodEdge.Ends
    game.Union.Unite ver1 ver2
    (ver1, ver2)
