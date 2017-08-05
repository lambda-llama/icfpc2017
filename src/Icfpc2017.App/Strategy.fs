module Strategy

type T = {
    name: string 
    init: Graph.T -> Game.State -> (Graph.VertexId * Graph.VertexId)
}

let stateless name f = { name = name; init = fun _ -> f }
let withSetup name setup step = { name = name; init = fun initialGraph -> let data = setup initialGraph in fun game -> step data game }

let private maxByWeight (graph: Graph.T) (weight: Graph.Edge -> int) =
    let { Graph.Ends = ends } =
        graph.Edges
            |> List.filter Graph.isUnclaimedEdge
            |> List.maxBy weight
    in ends

let randomEdge = 
    stateless "randomEdge" (fun game ->
        let { Graph.Ends = ends } = game.Graph.Edges |> List.find Graph.isUnclaimedEdge
        ends
    )

let growFromMines = 
    withSetup "growFromMines" ShortestPath.Compute (fun distances {Game.Graph=graph; Game.Me=me} ->
        let sources = Graph.sources graph in
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
                |> Seq.map (fun (_, ds) -> ds.[int next])
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
                Graph.outEdges graph mine
                (* TODO: pick the most remote one. *)
                |> List.find (fun _ -> true)
            in ends
    )        

let bruteForce1 = 
    withSetup "bruteForce1" ShortestPath.Compute (fun dists (game: Game.State) ->
        let graph = game.Graph
        let me = game.Me
        let isOurEdge {Graph.Color=cOpt} =
            match cOpt with
            | Some c -> c = me
            | None -> false
        in
        let weight (edge: Graph.Edge) =
            let graph = Graph.claimEdge graph me edge.Ends in
            let reach = ShortestPath.Compute {graph with Graph.Edges = List.filter isOurEdge graph.Edges} in
            let s = Game.score {game with Game.Graph = graph } dists reach in
            s
        in
        maxByWeight graph weight
    )

let makeNotEmpty xs = 0::xs 

let bruteForce3 = 
    withSetup "bruteForce3" ShortestPath.Compute (fun dists (game: Game.State) ->
        let graph = game.Graph
        let me = game.Me
        let isOurEdge {Graph.Color=cOpt} =
            match cOpt with
            | Some c -> c = me
            | None -> false
        in
        let weight (edge: Graph.Edge) =
            let graph = Graph.claimEdge graph me edge.Ends
            let yetUnclaimed = Graph.unclaimedEdges graph
            yetUnclaimed 
            |> List.map (fun edge -> 
                let graph = Graph.claimEdge graph me edge.Ends 
                let yetUnclaimed = Graph.unclaimedEdges graph
                yetUnclaimed 
                |> List.map (fun edge -> 
                    let graph = Graph.claimEdge graph me edge.Ends 
                    let reach = ShortestPath.Compute {graph with Graph.Edges = List.filter isOurEdge graph.Edges}
                    Game.score {game with Game.Graph = graph } dists reach
                )
                |> makeNotEmpty
                |> List.max
            )
            |> makeNotEmpty
            |> List.max
        in
        maxByWeight graph weight
    )        
