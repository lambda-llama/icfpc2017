module GreadyStrategy

open System

open Graphs
open FastUnion
open Union
open MinTreeEdgeScorer

let greedyStrategy: Strategy.T =
    Strategy.stateless "greedy" Map.empty (fun game ->
        let graph = game.Graph
        let bfs = Traversal.shortestPaths graph
        let dsu = FastUnion.create graph game.Me

        let getScore (edge: Edge.T) =
            let v1, v2 = Edge.ends edge
            if FastUnion.IsSameComponent dsu v1 v2
            then 0
            else getScore (FastUnion.getUnion dsu v1) (FastUnion.getUnion dsu v2) bfs
        Graph.unclaimedOrCanBuy graph |> Seq.maxBy getScore, game.StrategyState
    )

let greedyStrategyWithHeur: Strategy.T =
    Strategy.stateless "greedyHeur" Map.empty (fun game ->
        let graph = game.Graph
        let bfs = Traversal.shortestPaths graph
        let dsu = FastUnion.create graph game.Me

        let getScore (edge: Edge.T) =
            let v1, v2 = Edge.ends edge
            if FastUnion.IsSameComponent dsu v1 v2
            then 0
            else getScore (FastUnion.getUnion dsu v1) (FastUnion.getUnion dsu v2) bfs
        let candidates = Graph.unclaimedOrCanBuy graph |> Seq.map (fun x -> (x, getScore x))
        let maxGreedEdge = candidates |> Seq.maxBy snd
        let k, maxGreedValue = maxGreedEdge
        let bestCandidates = candidates |> Seq.filter (fun (a, b) -> b = maxGreedValue) |> Seq.map (fun (a, b)-> a)
        let winner = MinTreeEdgeScorer.GetBestEdge game.Me graph dsu bestCandidates
        winner, game.StrategyState
    )

let greedyRandomStrategy: Strategy.T =
    let r = Random ()
    Strategy.stateless "greedyRandom" Map.empty (fun game ->
        if r.NextDouble () <= 0.5
        then greedyStrategy.init game.Graph game
        else greedyStrategyWithHeur.init game.Graph game
    )

let isMinesConnectable (graph: Graph.T) (me:Color) (dsu: FastUnion.T) (mines: int seq) =
    let canTraverse edge = Graph.isClaimedBy me graph edge || not (Graph.isClaimed graph edge)
    mines |>
    Seq.exists (fun m ->
                let dist = Traversal.shortestPathWithPred graph m canTraverse
                mines |>
                Seq.exists (fun mm -> dsu.partition.find(mm) <> dsu.partition.find(m) && dist.[mm] <> -1)
    )

let printAllInfo (graph: Graph.T) (dsu: FastUnion.T) (bfs: Map<int ,int array>) (dist: int array) (start: int) =
    let V = Graph.nVertices graph
    let E = Graph.nEdges graph

    [0..E-1] |> List.map (fun i ->
                            let es = Graph.edges graph |> Array.ofSeq
                            let e = es.[i]
                            printfn "%A->%A" (Edge.ends e) (Graph.edgeColors graph e)) |> ignore
    printfn "Verginiti ="
    [0..V-1] |> List.map (fun i-> printf "%d isV %A; " i (FastUnion.IsVirginNode dsu i)) |> ignore
    printfn "\nDist from %d = " start
    dist |> Array.map (fun i-> printf "%d; " i) |> ignore
    printfn "\n= ==== = "

let getPromises (graph: Graph.T) (dsu: FastUnion.T) (bfs: Map<int ,int array>) (start: int) =
    let canTraverse edge =
        let u, v = Edge.ends edge
        let claimed = (Graph.isClaimed graph edge)
        let fromStart = ((u = start && IsVirginNode dsu v) || (v = start && IsVirginNode dsu u))
        let twoVergin = (IsVirginNode dsu v && IsVirginNode dsu u)
        let ans = (not claimed) &&  ( fromStart || twoVergin)
  //      printfn "dist %A->%A->%A claimed->%A fromStart->%A twoVergin->%A" (Edge.ends edge) (Graph.edgeColors graph edge) ans claimed fromStart twoVergin
        ans
    let dist = Traversal.shortestPathWithPred graph start canTraverse
//    printfn "+++++++ %A +++++" dist
//    printAllInfo graph dsu bfs dist start
    let mutable sum = 0
    let mines = getUnion dsu start
    for m in mines.Sources do
        for j in 0..Graph.nVertices graph - 1 do
            if dist.[j] <> -1
            then sum <- sum + bfs.[m].[j] * bfs.[m].[j]

    sum


let greedyStrategyWithHeur2: Strategy.T =
    Strategy.stateless "greedyHeur2" Map.empty (fun game ->
        let graph = game.Graph
        let bfs = Traversal.shortestPaths graph
        let dsu = FastUnion.create graph game.Me

        let getScore (edge: Edge.T) =
            let v1, v2 = Edge.ends edge
            if FastUnion.IsSameComponent dsu v1 v2
            then 0
            else getScore (FastUnion.getUnion dsu v1) (FastUnion.getUnion dsu v2) bfs
        let candidates = Graph.unclaimedOrCanBuy graph |> Seq.map (fun x -> (x, getScore x))
        let maxGreedEdge = candidates |> Seq.maxBy (fun (a, b) -> b)
        let k, maxGreedValue = maxGreedEdge
        //TODO: add options here
        let bestCandidates = candidates |> Seq.filter (fun (a, b) -> b = maxGreedValue) |> Seq.map (fun (a, b)-> a)
        if not (isMinesConnectable graph game.Me dsu (Graph.sources graph))
            then
                bestCandidates |>
                Seq.map (fun e ->
                            let u,v = Edge.ends e
                            let startForTraversal = if IsVirginNode dsu u then u else v
                            (e, startForTraversal)
                ) |>
                Seq.maxBy (fun (e, s) ->
                            let p = getPromises graph dsu bfs s
                            p) |>
                (fun (e,s)-> e) , game.StrategyState
            else
                let winner = MinTreeEdgeScorer.GetBestEdge game.Me graph dsu bestCandidates
                winner, game.StrategyState
    )
