module MinimaxStrategy

open System

open Game
open Graphs

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
                { state with Graph = Graph.claimEdge state.Graph player eid }
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

let isConnected (game: State) vid: bool =
    Graph.vertex game.Graph vid |> Vertex.isSource ||
        Graph.adjacentEdges game.Graph vid
        |> Seq.exists (Graph.isClaimed game.Graph)

let getUnclaimedEdges (game: State): Edge.T seq =
    Graph.unclaimed game.Graph
    |> Seq.filter (fun e ->
            let (a, b) = Edge.ends e
            isConnected game a || isConnected game b)

let getMoves (edgeWeights : int array) (game: State) (cap: int): Edge.T array =
    getUnclaimedEdges game
    |> Array.ofSeq
    |> Array.sortByDescending (Edge.id >> (fun id -> edgeWeights.[id]))
    |> Array.truncate cap

let heuristic (game: State): int =
    let graph = game.Graph
    let dists = Traversal.shortestPaths graph
    let reaches =
        {0..game.NumPlayers - 1}
        |> Seq.map (fun p -> Traversal.shortestPaths (Graph.subgraph graph p))
    let scores =
        reaches
        |> Seq.map (fun r -> Game.score2 game dists r)
        |> Seq.toArray
    let myScore = scores.[game.Me]
    scores.[game.Me] <- Int32.MinValue
    let bestOpponentScore = Array.max scores
    myScore - bestOpponentScore

let weighEdges (state: State): int array =
    Array.zeroCreate (Graph.nEdges state.Graph)

[<Literal>]
let EDGE_CAP = "edge-cap"
[<Literal>]
let DEPTH_CAP = "depth-cap"
[<Literal>]
let SCORING_CAP = "scoring-cap"
[<Literal>]
let TIMEOUTS_COUNT = "timeouts-count"

let defaultState =
    [
        EDGE_CAP, "30"
        DEPTH_CAP, "10"
        TIMEOUTS_COUNT, "0"
    ]|> Map.ofList

let defaultState2 =
    [
        EDGE_CAP, "30"
        SCORING_CAP, "2" // ? people * 2 moves
        DEPTH_CAP, "5" // ? people * 5 moves
        TIMEOUTS_COUNT, "0"
    ]|> Map.ofList

let minimax =
    Strategy.stateless "minimax" defaultState (fun game ->
        let mutable edgeCap = int game.StrategyState.[EDGE_CAP]
        let mutable depthCap = int game.StrategyState.[DEPTH_CAP]
        let mutable timeoutsCount = int game.StrategyState.[TIMEOUTS_COUNT]
        eprintfn "%d;%d;%d" edgeCap depthCap timeoutsCount
        let newStrategyState =
            if game.TimeoutsCount > 0 && edgeCap > 1 then
                timeoutsCount <- timeoutsCount + 1
                if timeoutsCount % 3 = 1 then
                    depthCap <- (depthCap + 1) / 2
                else
                    edgeCap <- (edgeCap + 1) / 2
                [
                    EDGE_CAP, string edgeCap
                    DEPTH_CAP, string depthCap
                    TIMEOUTS_COUNT, string timeoutsCount
                ]|> Map.ofList
            else
                game.StrategyState
        let maxDepth = Graph.unclaimed game.Graph |> Seq.length
        let depth = int (Math.Min(depthCap, maxDepth))
        let m =
            Minimax.create
                heuristic
                (fun s _ -> getMoves (weighEdges game) s edgeCap)
                (fun p -> p = game.Me)
        let edge =
            Minimax.run m game game.Me depth
            |> Array.maxBy (fun (_, s) -> s)
            |> fst
            |> Option.get
        (edge, newStrategyState)
    )

let minimax2 =
    Strategy.stateless "minimax2" defaultState2 (fun game ->
        let mutable edgeCap = int game.StrategyState.[EDGE_CAP]
        let mutable scoringCap = int game.StrategyState.[SCORING_CAP]
        let mutable depthCap = int game.StrategyState.[DEPTH_CAP]
        let mutable timeoutsCount = int game.StrategyState.[TIMEOUTS_COUNT]
        eprintfn "%d;%d;%d;%d" edgeCap scoringCap depthCap timeoutsCount
        let newStrategyState =
            if game.TimeoutsCount > 0 && edgeCap > 1 then
                timeoutsCount <- timeoutsCount + 1
                if timeoutsCount = 1 then
                    scoringCap <- 1
                elif timeoutsCount = 2 then
                    depthCap <- 3
                elif timeoutsCount = 3 then
                    edgeCap <- edgeCap / 2
                elif timeoutsCount = 4 then
                    depthCap <- 2
                elif timeoutsCount = 5 then
                    edgeCap <- edgeCap / 2
                elif timeoutsCount = 6 then
                    scoringCap <- 0
                else
                    edgeCap <- (edgeCap + 1) / 2
                [
                    EDGE_CAP, string edgeCap
                    SCORING_CAP, string scoringCap
                    DEPTH_CAP, string depthCap
                    TIMEOUTS_COUNT, string timeoutsCount
                ]|> Map.ofList
            else
                game.StrategyState
        let maxDepth = Graph.unclaimed game.Graph |> Seq.length
        // Find worst enemy - our move is the first, so try to maximize each enemy's potential threat
        let depth = int (Math.Min(game.NumPlayers * scoringCap, maxDepth))
        let setups =
            [0..game.NumPlayers - 1]
            |> List.filter (fun p -> p <> game.Me)
            |> List.map (fun player ->
                (player, Minimax.create
                    heuristic
                    (fun s p ->
                        if p = game.Me then [||]
                        else getMoves (weighEdges game) s edgeCap)
                    (fun p -> p = player)))
        let scores =
            setups
            |> List.map (fun (p, m) -> (p, Minimax.run m game game.Me depth |> Array.map snd |> Array.max))
        let worstEnemy =
            scores
            |> List.maxBy snd
            |> fst
        let depth = int (Math.Min(game.NumPlayers * depthCap, maxDepth))
        let m =
            Minimax.create
                heuristic
                (fun s p ->
                    if p <> game.Me && p <> worstEnemy then [||]
                    else getMoves (weighEdges game) s edgeCap)
                (fun p -> p = game.Me)
        let edge =
            Minimax.run m game game.Me depth
            |> Array.maxBy (fun (_, s) -> s)
            |> fst
            |> Option.get
        (edge, newStrategyState)
    )
