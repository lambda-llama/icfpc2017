module Strategy

type T = Game.State -> (Graph.VertexId * Graph.VertexId)

let private maxByWeight (graph: Graph.T) (weight: Graph.Edge -> int) =
    let { Graph.Ends = ends } =
        graph.Edges
            |> List.filter Graph.isUnclaimedEdge
            |> List.maxBy weight
    in ends

let randomEdge: T = fun game ->
    let { Graph.Ends = ends } = game.Graph.Edges |> List.find Graph.isUnclaimedEdge in
    ends

let growFromMines: T = fun {Game.Graph=graph; Game.Me=me} ->
    let attachedToMine edge = Array.exists (Graph.isEndPoint edge) graph.Sources in
    let attachedToOurEdge { Graph.Ends = (u, v) } =
        Seq.append (Graph.outEdges graph u) (Graph.outEdges graph v)
        |> Seq.exists (fun {Graph.Color = c} ->
            match c with
            | Some(c) -> c = me
            | _ -> false
        )
    in
    let weight edge = if attachedToOurEdge edge || attachedToMine edge then 1 else 0 in
    maxByWeight graph weight

let all = 
    [("randomEdge", randomEdge); 
     ("growFromMines", growFromMines)]
    |> Map.ofList 
