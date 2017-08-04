module Strategy

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
    let attachedToMine edge = Array.exists (Graph.isEndPoint edge) game.Graph.Sources in
    let attachedToOurEdge { Graph.Ends = (u, v) } =
        Seq.append (Graph.outEdges game.Graph u) (Graph.outEdges game.Graph u)
        |> Seq.exists (fun {Graph.Color = c} ->
            match c with
            | Some(c) -> c = game.Me
            | _ -> false
        )
    in
    let weight edge = if attachedToMine edge || attachedToOurEdge edge then 1 else 0 in
    maxByWeight game weight