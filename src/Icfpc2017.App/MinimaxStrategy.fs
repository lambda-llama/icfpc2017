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

let getUnclaimedEdges (game: State): Edge list =
    game.Graph.Edges
    |> List.filter isUnclaimedEdge
    |> List.filter (fun { Ends = (a, b) } -> isConnected game a || isConnected game b)

let heuristic (game: State): int =
    let graph = game.Graph
    let dists = ShortestPath.Compute graph
    let reaches =
        [0..game.NumPlayers - 1]
        |> List.map
            (fun p ->
                let isOwnedEdge { Graph.Color = cOpt } =
                    match cOpt with
                    | Some c -> c = uint32 p
                    | None -> false
                ShortestPath.Compute { graph with Graph.Edges = List.filter isOwnedEdge graph.Edges })
    let scores =
        reaches
        |> List.map (fun r -> Game.score game dists r)
        |> List.toArray
    let myScore = scores.[int game.Me]
    scores.[int game.Me] <- Int32.MinValue
    let bestOpponentScore = scores |> Array.max
    myScore - bestOpponentScore

let rec _minimax (state: State) (edge: Edge) (player: Color) (depth: int): int =
    let { Ends = ends } = edge
    let newState = { state with Graph = claimEdge state.Graph player ends }
    let nextPlayer = (player + 1u) % uint32 state.NumPlayers
    
    if (depth = 0) then
        heuristic newState
    elif player = newState.Me then
        let mutable best = Int32.MinValue
        for newEdge in (getUnclaimedEdges newState) do
            let value = _minimax newState newEdge nextPlayer (depth - 1)
            best <- if value > best then value else best
        best
    else
        let mutable best = Int32.MaxValue
        for newEdge in (getUnclaimedEdges newState) do
            let value = _minimax newState newEdge nextPlayer (depth - 1)
            best <- if value < best then value else best
        best

let minimax: Strategy.T = fun game ->
    let maxDepth = 1
    let depth = int (Math.Min(maxDepth, game.Graph.Edges |> List.filter isUnclaimedEdge |> List.length))
    getUnclaimedEdges game
    |> List.sortByDescending (fun e -> _minimax game e game.Me depth)
    |> List.find (fun _ -> true)
    |> fun { Ends = ends } -> ends
