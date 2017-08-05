module MinimaxStrategy

open System
open Game
open Graphs

let isSource (graph: Graphs.Graph.T) vertex =
    (Graphs.Graph.vertices graph).[vertex].IsSource

let outEdges (graph: Graphs.Graph.T) vertex: Edge.T array =
    (Graph.edges graph)
    |> Array.filter (fun e -> Edge.contains e vertex)

let isUnclaimedEdge (graph: Graphs.Graph.T) (edge: Edge.T): bool =
    Graph.isClaimed graph edge |> not

let isConnected (game: State) vertex: bool =
    isSource game.Graph2 vertex ||
        outEdges game.Graph2 vertex
        |> Array.exists (isUnclaimedEdge game.Graph2 >> not)

let getUnclaimedEdges (game: State): Edge.T array =
    (Graphs.Graph.edges game.Graph2)
    |> Array.filter (isUnclaimedEdge game.Graph2)
    |> Array.filter
        (fun e ->
            let (a, b) = Edge.ends e
            isConnected game a || isConnected game b)

let heuristic (game: State): int =
    let graph = game.Graph2
    let dists = Traversal.shortestPaths graph
    let reaches =
        [0..game.NumPlayers - 1]
        |> List.map
            (fun p ->
                Traversal.shortestPaths (Graph.withEdges graph (Array.filter (Graph.isClaimedBy p graph) (Graph.edges graph))))
    let scores =
        reaches
        |> List.map (fun r -> Game.score2 game dists r)
        |> List.toArray
    let myScore = scores.[game.Me]
    scores.[game.Me] <- Int32.MinValue
    let bestOpponentScore = scores |> Array.max
    myScore - bestOpponentScore

let rec _minimax (state: State) (edge: Edge.T) (player: Color) (depth: int) (alpha: int) (beta: int): int =
    let eid = Edge.id edge
    let newState = { state with Graph2 = Graph.claimEdge state.Graph2 player eid }
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

let minimax = 
    Strategy.stateless "minimax" (fun game ->
        let maxDepth = 10
        let depth = int (Math.Min(maxDepth, (Graph.edges game.Graph2) |> Array.filter (isUnclaimedEdge game.Graph2) |> Array.length))
        getUnclaimedEdges game
        |> Array.sortByDescending (fun e -> _minimax game e game.Me depth Int32.MinValue Int32.MaxValue)
        |> Array.find (fun _ -> true)
    )
