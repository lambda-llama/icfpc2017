module MinimaxStrategy

open System
open Game
open Graphs

let isSource (graph: Graphs.Graph.T) vertex =
    (Graphs.Graph.vertices graph).[vertex] |> Vertex.isSource

let outEdges (graph: Graphs.Graph.T) vertex: Edge.T array =
    (Graph.edges graph)
    |> Array.filter (fun e -> Edge.contains e vertex)

let isUnclaimedEdge (graph: Graphs.Graph.T) (edge: Edge.T): bool =
    Graph.isClaimed graph edge |> not

let isConnected (game: State) vertex: bool =
    isSource game.Graph2 vertex ||
        outEdges game.Graph2 vertex
        |> Array.exists (isUnclaimedEdge game.Graph2 >> not)

let getUnclaimedEdges (game: State) (player: Color): Edge.T array =
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
        |> List.map (fun p -> Traversal.shortestPaths (Graph.subgraph graph p))
    let scores =
        reaches
        |> List.map (fun r -> Game.score2 game dists r)
        |> List.toArray
    let myScore = scores.[game.Me]
    scores.[game.Me] <- Int32.MinValue
    let bestOpponentScore = scores |> Array.max
    myScore - bestOpponentScore

module Minimax =
    type T = private {
        score: State -> int
        getMoves: State -> Color -> Edge.T option array
        isMaximizing: Color -> bool
    }

    let create score (getMoves: State ->Color -> Edge.T array) isMaximizing =
        {
            score = score
            getMoves = (fun s p ->
                let moves = getMoves s p
                if moves.Length = 0 then [|None|] else moves |> Array.map Some
            )
            isMaximizing = isMaximizing
        }

    let rec private minimax (m: T) (state: State) (edge: Edge.T option) (player: Color) (depth: int) (alpha: int) (beta: int): int =
        let newState =
            match edge with
            | Some edge ->
                let eid = Edge.id edge
                { state with Graph2 = Graph.claimEdge state.Graph2 player eid }
            | None -> state // pass
        let nextPlayer = (player + 1) % state.NumPlayers

        let min x y = if x < y then x else y
        let max x y = if x > y then x else y

        if (depth = 0) then
            m.score newState
        elif m.isMaximizing player then
            let mutable best = Int32.MinValue
            let mutable a = alpha
            let edges = m.getMoves newState player
            let mutable i = 0
            while (i = 0 || beta <= a) && i < edges.Length do
                let value = minimax m newState edges.[i] nextPlayer (depth - 1) a beta
                best <- max best value
                a <- max a best
                i <- i + 1
            best
        else
            let mutable best = Int32.MaxValue
            let mutable b = beta
            let edges = m.getMoves newState player
            let mutable i = 0
            while (i = 0 || b <= alpha) && i < edges.Length do
                let value = minimax m newState edges.[i] nextPlayer (depth - 1) alpha b
                best <- min best value
                b <- min b best
                i <- i + 1
            best

    let run (m: T) (state: State) (player: Color) (depth: int): (Edge.T option * int) array =
        m.getMoves state player
        |> Array.map (fun e -> (e, minimax m state e player depth Int32.MinValue Int32.MaxValue))

let minimax =
    Strategy.stateless "minimax" (fun game ->
        let maxDepth =
            (Graph.edges game.Graph2)
            |> Array.filter (isUnclaimedEdge game.Graph2)
            |> Array.length
        let depth = int (Math.Min(10, maxDepth))
        let m =
            Minimax.create
                heuristic
                getUnclaimedEdges
                (fun p -> p = game.Me)
        Minimax.run m game game.Me depth
        |> Array.maxBy (fun (_, s) -> s)
        |> fst
        |> Option.get
    )

let minimax2 =
    Strategy.stateless "minimax2" (fun game ->
        let maxDepth =
            (Graph.edges game.Graph2)
            |> Array.filter (isUnclaimedEdge game.Graph2)
            |> Array.length
        // Find worst enemy - our move is the first, so try to maximize each enemy's potential threat
        let depth = int (Math.Min(game.NumPlayers * 2, maxDepth))
        let setups =
            [0..game.NumPlayers - 1]
            |> List.filter (fun p -> p <> game.Me)
            |> List.map (fun player ->
                (player, Minimax.create
                    heuristic
                    (fun s p -> if p = game.Me then [||] else getUnclaimedEdges s p)
                    (fun p -> p = player)))
        let scores =
            setups
            |> List.map (fun (p, m) -> (p, Minimax.run m game game.Me depth |> Array.map snd |> Array.max))
        let worstEnemy =
            scores
            |> List.maxBy snd
            |> fst
        let depth = int (Math.Min(game.NumPlayers * 5, maxDepth))
        let m =
            Minimax.create
                heuristic
                (fun s p -> if p <> game.Me && p <> worstEnemy then [||] else getUnclaimedEdges s p)
                (fun p -> p = game.Me)
        Minimax.run m game game.Me depth
        |> Array.maxBy (fun (_, s) -> s)
        |> fst
        |> Option.get
    )
