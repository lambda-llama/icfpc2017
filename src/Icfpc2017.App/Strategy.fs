module Stragy

type T = Graph.T -> (Graph.VertexId * Graph.VertexId)

let randomEdge: T = fun graph ->
    let { Graph.Ends = ends } = graph.Edges |> List.find Graph.isUnclaimedEdge in
    ends

