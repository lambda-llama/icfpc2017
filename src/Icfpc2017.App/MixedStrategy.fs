module MixedStrategy

open MinimaxStrategy
open Strategy
open Graph
open Game
open GreadyStrategy

let private mix name cutoff (slow: Strategy.T) (fast: Strategy.T): Strategy.T = {
    Strategy.name = name;
    Strategy.init = fun graph ->
        let s = slow.init graph
        let f = fast.init graph
        fun (game: Game.State) -> 
            let graph = game.Graph
            let edges = graph.Edges
            let nFreeEdges = edges |> List.filter (fun edge -> edge.Color <> None ) |> List.length
            if nFreeEdges > cutoff then
                f game 
            else
                s game
}


let mixedStrategy: Strategy.T = 
    mix "mixed" 5 minimax growFromMines
