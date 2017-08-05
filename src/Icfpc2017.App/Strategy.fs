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
    let distances = ShortestPath.Compute graph in
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

    if List.exists isOurEdge graph.Edges
    then maxByWeight graph weight
    else
        let mine =
            distances |> Map.toSeq
            |> Seq.map (fun (mine, ds) ->
                (mine, Array.fold (fun acc x -> acc + x*x) 0 ds))
            |> Seq.maxBy (fun (_, distance) -> distance)
            |> fst
        in

        let {Graph.Ends=ends} =
            Graph.outEdges graph (uint32 mine)
            (* TODO: pick the most remote one. *)
            |> List.find (fun _ -> true)
        in ends

let bruteForceOneStep: T = fun game ->
    let graph = game.Graph
    let me = game.Me
    let isOurEdge {Graph.Color=cOpt} =
        match cOpt with
        | Some c -> c = me
        | None -> false
    in
    let dists = ShortestPath.Compute graph in
    let weight (edge: Graph.Edge) =
        let graph = Graph.claimEdge graph me edge.Ends in
        let reach = ShortestPath.Compute {graph with Graph.Edges = List.filter isOurEdge graph.Edges} in
        let s = Game.score {game with Game.Graph = graph } dists reach in
        s
    in
    maxByWeight graph weight
