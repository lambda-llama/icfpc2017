module Strategy

open Graphs

type T = {
    name: string
    init: Graph.T -> Game.State -> Edge.T
}

let stateless name f = { name = name; init = fun _ -> f }
let withSetup name setup step = { name = name; init = fun initialGraph -> let data = setup initialGraph in fun game -> step data game }

let private maxByWeight (graph: Graph.T) (weight: Edge.T -> 'a when 'a: comparison) =
    Graph.unclaimed graph |> Seq.maxBy weight

let randomEdge =
    stateless "randomEdge" (fun game ->
        Graph.unclaimed game.Graph |> Seq.head
    )

let growFromMines =
    withSetup "growFromMines" Traversal.shortestPaths (fun distances {Game.Graph=graph; Game.Me=me} ->
        let sources = Graph.sources graph in
        let isOurEdge = Graph.isClaimedBy me graph

        let attachedToMine edge = Seq.exists (Edge.contains edge) sources in
        let attachedToOurEdge v = Graph.adjacentEdges graph v |> Seq.exists isOurEdge in

        let weight (edge) =
            let (u, v) = Edge.ends edge
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

        if Array.exists isOurEdge (Graph.edges graph)
        then maxByWeight graph weight
        else
            let mine =
                distances |> Map.toSeq
                |> Seq.map (fun (mine, ds) ->
                    (mine, Array.fold (fun acc x -> acc + x*x) 0 ds))
                |> Seq.maxBy (fun (_, distance) -> distance)
                |> fst
            in

            Graph.adjacentEdges graph mine
            (* TODO: pick the most remote one. *)
            |> Seq.find (fun _ -> true)
    )

let bruteForce1 =
    withSetup "bruteForce1" Traversal.shortestPaths (fun dists (game: Game.State) ->
        let graph = game.Graph
        let me = game.Me
        let weight (edge: Edge.T) =
            let graph = Graph.claimEdge graph me (Edge.id edge)
            let reach = Traversal.shortestPaths (Graph.subgraph graph me)
            Game.score2 {game with Game.Graph = graph } dists reach
        in
        maxByWeight graph weight
    )

let makeNotEmpty xs = Seq.append xs (Seq.ofList [0])

let bruteForce3 =
    withSetup "bruteForce3" Traversal.shortestPaths (fun dists (game: Game.State) ->
        let graph = game.Graph
        let me = game.Me
        let weight (edge: Edge.T) =
            let graph = Graph.claimEdge graph me (Edge.id edge)
            let reach = Traversal.shortestPaths (Graph.subgraph graph me)
            let tieScore = Game.score2 {game with Game.Graph = graph } dists reach
            Graph.unclaimed graph
            |> Seq.map (fun edge ->
                let graph = Graph.claimEdge graph me (Edge.id edge)
                Graph.unclaimed graph
                |> Seq.map (fun edge ->
                    let graph = Graph.claimEdge graph me (Edge.id edge)
                    let reach = Traversal.shortestPaths (Graph.subgraph graph me)
                    Game.score2 {game with Game.Graph = graph } dists reach
                )
                |> makeNotEmpty
                |> Seq.max
            )
            |> makeNotEmpty
            |> Seq.max
            |> (fun score -> (score, tieScore))
        in
        maxByWeight graph weight
    )