module Strategy

module Graph2 = Graphs.Graph
module Edge2 = Graphs.Edge

type T = {
    name: string 
    init: Graphs.Graph.T -> Game.State -> Graphs.Edge.T
}

let stateless name f = { name = name; init = fun _ -> f }
let withSetup name setup step = { name = name; init = fun initialGraph -> let data = setup initialGraph in fun game -> step data game }

let private maxByWeight (graph: Graphs.Graph.T) (weight: Graphs.Edge.T -> int) =
    Graphs.Graph.unclaimed graph |> Seq.maxBy weight

let randomEdge = 
    stateless "randomEdge" (fun game ->
        Graphs.Graph.unclaimed game.Graph2 |> Seq.head
    )

let growFromMines = 
    withSetup "growFromMines" Graphs.Traversal.shortestPaths (fun distances {Game.Graph2=graph; Game.Me=me} ->
        let sources = Graphs.Graph.sources graph in
        let isOurEdge = Graphs.Graph.isClaimedBy me graph

        let attachedToMine edge = Seq.exists (Graphs.Edge.contains edge) sources in
        let attachedToOurEdge v = Graphs.Graph.adjacentEdges graph v |> Seq.exists isOurEdge in

        let weight (edge) =
            let (u, v) = Graphs.Edge.ends edge
            let uIsOurs = attachedToOurEdge u
            let vIsOurs = attachedToOurEdge v
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

        if Array.exists isOurEdge (Graphs.Graph.edges graph)
        then maxByWeight graph weight
        else
            let mine =
                distances |> Map.toSeq
                |> Seq.map (fun (mine, ds) ->
                    (mine, Array.fold (fun acc x -> acc + x*x) 0 ds))
                |> Seq.maxBy (fun (_, distance) -> distance)
                |> fst
            in

            Graphs.Graph.adjacentEdges graph mine
            (* TODO: pick the most remote one. *)
            |> Seq.find (fun _ -> true)
    )        

let bruteForce1 = 
    withSetup "bruteForce1" Graphs.Traversal.shortestPaths (fun dists (game: Game.State) ->
        let graph = game.Graph2
        let me = game.Me
        let weight (edge: Graphs.Edge.T) =
            let graph = Graphs.Graph.claimEdge graph me (Graphs.Edge.id edge)
            let reach = Graphs.Traversal.shortestPaths (Graphs.Graph.subgraph graph me)
            Game.score {game with Game.Graph2 = graph } dists reach
        in
        maxByWeight graph weight
    )

let makeNotEmpty xs = Seq.append xs (Seq.ofList [0])

let bruteForce3 =
    withSetup "bruteForce3" Graphs.Traversal.shortestPaths (fun dists (game: Game.State) ->
        let graph = game.Graph2
        let me = game.Me
        let weight (edge: Graphs.Edge.T) =
            let graph = Graphs.Graph.claimEdge graph me (Graphs.Edge.id edge)
            Graphs.Graph.unclaimed graph
            |> Seq.map (fun edge -> 
                let graph = Graphs.Graph.claimEdge graph me (Graphs.Edge.id edge)
                Graphs.Graph.unclaimed graph
                |> Seq.map (fun edge -> 
                    let graph = Graphs.Graph.claimEdge graph me (Graphs.Edge.id edge)
                    let reach = Graphs.Traversal.shortestPaths (Graphs.Graph.subgraph graph me)
                    Game.score {game with Game.Graph2 = graph } dists reach
                )
                |> makeNotEmpty
                |> Seq.max
            )
            |> makeNotEmpty
            |> Seq.max
        in
        maxByWeight graph weight
    )