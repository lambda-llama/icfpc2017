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
    let sources = Graph.sources graph in
    let distances = ShortestPath.Compute graph sources in
    let isOurEdge {Graph.Color=cOpt} =
        match cOpt with 
        | Some c -> c = me
        | None -> false
    in        

    let attachedToMine edge = Seq.exists (Graph.isEndPoint edge) sources in
    let attachedToOurEdge v = Graph.outEdges graph v |> Seq.exists isOurEdge in

    let weight ({Graph.Ends = (u, v) } as edge) =
        let uIsOurs = attachedToOurEdge u in
        let vIsOurs = attachedToOurEdge v in 
        if uIsOurs || vIsOurs
        then 
            let next = if vIsOurs then u else v in
            distances 
            |> Map.toSeq
            |> Seq.map (fun (mine, ds) -> ds.[int next])
            |> Seq.min
        else if attachedToMine edge then 1
        else 0
    in

    maxByWeight graph weight

let all =
    [("randomEdge", randomEdge);
     ("growFromMines", growFromMines)]
    |> Map.ofList
