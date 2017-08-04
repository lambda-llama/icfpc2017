module Stragy

type T = Game.State -> (Graph.VertexId * Graph.VertexId)

let private maxByWeight (game: Game.State) (weight: Graph.Edge -> int) =
    let { Graph.Ends = ends } = 
        game.Graph.Edges 
            |> List.filter Graph.isUnclaimedEdge
            |> List.maxBy weight
    in ends
let randomEdge: T = fun game ->
    let { Graph.Ends = ends } = game.Graph.Edges |> List.find Graph.isUnclaimedEdge in
    ends

let growFromMines (game: Game.State) = 
    let weight edge = if List.exists (Graph.isEndPoint edge)  game.Mines then 1 else 0 in 
    maxByWeight game weight