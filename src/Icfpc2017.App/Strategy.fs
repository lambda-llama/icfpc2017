module Strategy

open Graphs
open System
open System.Threading
open System.Threading.Tasks

type T = {
    name: string
    init: Graph.T -> Game.State -> Edge.T * Map<string, string>
}

let stateless name step = { name = name; init = fun _ -> step }
let withSetup name setup step = { name = name; init = fun initialGraph -> let data = setup initialGraph in fun game -> step data game }

let mixSlowFastTimeout name (timeoutMs: int) (slow: T) (fast: T) = {
    name = name;
    init = fun initialGraph ->
        let s = slow.init initialGraph
        let f = fast.init initialGraph
        fun game ->
            let stopWatch = System.Diagnostics.Stopwatch.StartNew()
            let result = 
                async {
                    let fastTask = async { return Some(f game) }
                    let slowTask = async { return Some(Some(s game)) }
                    let timtoutTask = async {
                        do! Async.Sleep timeoutMs
                        return Some(None)
                    }
                    let timedTask = async {
                        let! c = Async.Choice [timtoutTask; slowTask]
                        return (Option.get c)
                    }
                    let! xs = [ timedTask; fastTask ] |> Async.Parallel
                    printf "%s\n" (if Option.isSome xs.[0] then "SLOW" else "FAST")
                    return (Array.choose id xs).[0]
                } |> Async.RunSynchronously
            stopWatch.Stop()
            printfn "\nTime for turn: %f\n" stopWatch.Elapsed.TotalMilliseconds
            result
}

let private maxByWeight (graph: Graph.T) (weight: Edge.T -> 'a when 'a: comparison) =
    Graph.unclaimed graph |> Seq.maxBy weight

let randomEdge =
    stateless "randomEdge" (fun game ->
        (Graph.unclaimed game.Graph |> Seq.head, Map.empty)
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

        let edge =
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
        (edge, Map.empty)
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
        (maxByWeight graph weight, Map.empty)
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
        (maxByWeight graph weight, Map.empty)
    )

let combinedForce = 
    mixSlowFastTimeout "combinedForce" 500 bruteForce3 bruteForce1
