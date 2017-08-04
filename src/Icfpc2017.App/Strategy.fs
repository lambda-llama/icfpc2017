module Stragy

type T = Game.State -> (Graph.VertexId * Graph.VertexId)
            
let randomEdge: T = fun game ->
    let { Graph.Ends = ends } = game.Graph.Edges |> List.find Graph.isUnclaimedEdge in
    ends

