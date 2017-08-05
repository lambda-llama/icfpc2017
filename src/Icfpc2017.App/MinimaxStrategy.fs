module MinimaxStrategy

open System
open System.Collections.Generic
open Game
open Graph
open Strategy

let isConnected (game: State) vertex: bool =
    isSource game.Graph vertex ||
        outEdges game.Graph vertex
        |> List.exists (isUnclaimedEdge >> not)

let getUnclaimedEdges (game: State): Edge array =
    game.Graph.Edges
    |> List.filter isUnclaimedEdge
    |> List.filter (fun { Ends = (a, b) } -> isConnected game a || isConnected game b)
    |> List.toArray

let heuristic (game: State): int =
    let graph = game.Graph
    let dists = ShortestPath.Compute graph
    let reaches =
        [0..game.NumPlayers - 1]
        |> List.map
            (fun p ->
                let isOwnedEdge { Graph.Color = cOpt } =
                    match cOpt with
                    | Some c -> c = p
                    | None -> false
                ShortestPath.Compute { graph with Graph.Edges = List.filter isOwnedEdge graph.Edges })
    let scores =
        reaches
        |> List.map (fun r -> Game.score game dists r)
        |> List.toArray
    let myScore = scores.[game.Me]
    scores.[game.Me] <- Int32.MinValue
    let bestOpponentScore = scores |> Array.max
    myScore - bestOpponentScore

let rec _minimax (state: State) (edge: Edge) (player: Color) (depth: int) (alpha: int) (beta: int): int =
    let { Ends = ends } = edge
    let newState = { state with Graph = claimEdge state.Graph player ends }
    let nextPlayer = (player + 1) % state.NumPlayers
    
    let min x y = if x < y then x else y
    let max x y = if x > y then x else y

    if (depth = 0) then
        heuristic newState
    elif player = newState.Me then
        let mutable best = Int32.MinValue
        let mutable a = alpha
        let edges = getUnclaimedEdges newState
        let mutable i = 0
        while (i = 0 || beta <= a) && i < edges.Length do
            let value = _minimax newState edges.[i] nextPlayer (depth - 1) a beta
            best <- max best value
            a <- max a best
            i <- i + 1
        best
    else
        let mutable best = Int32.MaxValue
        let mutable b = beta
        let edges = getUnclaimedEdges newState
        let mutable i = 0
        while (i = 0 || b <= alpha) && i < edges.Length do
            let value = _minimax newState edges.[i] nextPlayer (depth - 1) alpha b
            best <- min best value
            b <- min b best
            i <- i + 1
        best

let minimax: Strategy.T = fun game ->
    let maxDepth = 0
    let depth = int (Math.Min(maxDepth, game.Graph.Edges |> List.filter isUnclaimedEdge |> List.length))
    getUnclaimedEdges game
    |> Array.sortByDescending (fun e -> _minimax game e game.Me depth Int32.MinValue Int32.MaxValue)
    |> Array.find (fun _ -> true)
    |> fun { Ends = ends } -> ends
