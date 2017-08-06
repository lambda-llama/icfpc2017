module GreadyStrategy

open Graphs
open FastUnion
open Union

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
        Graph.unclaimed graph |> Seq.maxBy getScore, game.StrategyState
    )