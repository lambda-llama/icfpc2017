module GreadyStrategy

open Graphs
open FastUnion
open Union
open MinTreeEdgeScorer

let greadyStrategy: Strategy.T =
    Strategy.stateless "gready" Map.empty (fun game ->
        let graph = game.Graph
        let bfs = Traversal.shortestPaths graph
        let dsu = FastUnion.create graph game.Me

        let getScore (edge: Edge.T) =
            let v1, v2 = Edge.ends edge
            if FastUnion.IsSameComponent dsu v1 v2
            then 0
            else getScore (FastUnion.getUnion dsu v1) (FastUnion.getUnion dsu v2) bfs
        Graph.unclaimedOrCanBuy graph |> Seq.maxBy getScore, game.StrategyState
    )

let greadyStrategyWithHeur: Strategy.T =
    Strategy.stateless "gready_heur" Map.empty (fun game ->
        let graph = game.Graph
        let bfs = Traversal.shortestPaths graph
        let dsu = FastUnion.create graph game.Me

        let getScore (edge: Edge.T) =
            let v1, v2 = Edge.ends edge
            if FastUnion.IsSameComponent dsu v1 v2
            then 0
            else getScore (FastUnion.getUnion dsu v1) (FastUnion.getUnion dsu v2) bfs
        let candidates = Graph.unclaimedOrCanBuy graph |> Seq.map (fun x -> (x, getScore x))
        let maxGreedEdge = candidates |> Seq.maxBy snd
        let k, maxGreedValue = maxGreedEdge
        let bestCandidates = candidates |> Seq.filter (fun (a, b) -> b = maxGreedValue) |> Seq.map (fun (a, b)-> a)
        let winner = MinTreeEdgeScorer.GetBestEdge game.Me graph dsu bestCandidates
        winner, game.StrategyState
    )