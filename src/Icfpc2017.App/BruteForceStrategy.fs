module BruteForceStrategy

open Graphs

let bruteForce1 =
    Strategy.withSetup "bruteForce1" Traversal.shortestPaths (fun dists (game: Game.State) ->
        let graph = game.Graph
        let me = game.Me
        let weight (edge: Edge.T) =
            let graph = Graph.claimEdge graph me (Edge.id edge)
            let reach = Traversal.shortestPaths (Graph.subgraph graph me)
            Game.score2 {game with Game.Graph = graph } dists reach
        in
        (Graph.unclaimed graph |> Seq.maxBy weight, Map.empty)
    )

let makeNotEmpty xs = Seq.append xs (Seq.ofList [0])

let bruteForce3 =
    Strategy.withSetup "bruteForce3" Traversal.shortestPaths (fun dists (game: Game.State) ->
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
        (Graph.unclaimed graph |> Seq.maxBy weight, Map.empty)
    )

let combinedForce =
    Strategy.mixSlowFastTimeout "combinedForce" 500 bruteForce3 bruteForce1
