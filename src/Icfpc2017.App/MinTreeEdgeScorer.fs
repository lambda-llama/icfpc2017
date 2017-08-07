module MinTreeEdgeScorer

open Graphs
open FastUnion

let getCentreValue (sources: int[]) (uwe: UnionWithEdge) (dist: int[])  =
    let nVert = dist.Length
    let compDist = Array.create nVert 99999
    for i in 0..nVert-1 do
        let p = getUWEParent uwe i
        let d = if dist.[i] = -1
                    then 99999
                    else dist.[i]
        compDist.[p] <- min compDist.[p] d
    let mutable sum = 0
    for i in sources do
        sum <- sum + compDist.[i]
    sum

let ScoreOneEdge (sources: int[]) (uf: FastUnion.T) (pathes: int array[]) (uv:int*int) =
    let u,v = uv
    let UWE = createUnionWithEdge uf u v
    pathes |> Array.map (fun x -> getCentreValue sources UWE x) |> Array.min

let GetBestEdge (me: Color) (graph: Graph.T) (uf: FastUnion.T) (edges: Edge.T seq) =
    let rnd = System.Random()
    let randomCenters = [|for i in 0..100 do yield rnd.Next (Graph.nVertices graph)|] |> Array.distinct
    let canTraverse edge =
        Graph.canBuy graph edge ||
        Graph.isClaimedBy me graph edge ||
        not (Graph.isClaimed graph edge)
    let pathes = randomCenters |> Array.map (fun x-> Traversal.shortestPathWithPred graph x canTraverse)
    let sources = Graph.sources graph |> Seq.toArray
    let maxEdge, value = edges |>
                            Seq.map (fun x-> x, ScoreOneEdge sources uf pathes (Edge.ends x)) |>
                            Seq.maxBy (fun (a, b) -> b)
    maxEdge
