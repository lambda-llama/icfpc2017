module MixedStrategy

open MinimaxStrategy
open Strategy
open Graph
open Game
open GreadyStrategy

let mixedStrategy: Strategy.T = fun game ->
    let graph = game.Graph
    let me = game.Me
    let edges = graph.Edges
    let nFreeEdges = edges |> List.filter (fun edge -> edge.Color <> None ) |> List.length
    if nFreeEdges > 5 then
        growFromMines game 
    else
        minimax game