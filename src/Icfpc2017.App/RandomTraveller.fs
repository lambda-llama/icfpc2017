module RandomTraveller

open Graphs
open FastUnion
// Never checked to work
let rec travelFromVertex (graph: Graph.T) (steps: int) (path: Edge.T list) (vertex: int) =
    if steps = 0 then
        (0, path) // The end; TODO: calc the score
    else
        let candidateEdges = 
            Graph.unclaimed graph
            |> Seq.filter (fun edge ->  Edge.contains edge vertex)
            |> Seq.filter (fun edge -> not (List.contains edge path))
        let nCandidateEdges = candidateEdges |> Seq.length
        if nCandidateEdges = 0 then
            (0, path) // nowhere to go TODO: add actual score computatuin
        else
            let rnd = System.Random()
            let nextEdge = rnd.Next nCandidateEdges |> List.nth (Seq.toList candidateEdges)
            let ends = Edge.ends nextEdge
            let nextVertex = Edge.opposite nextEdge vertex
            let (finalScore, furtherPath) = travelFromVertex graph (steps - 1) ([nextEdge] @ path) nextVertex
            (finalScore, nextEdge :: furtherPath)

(*

let GetBestEdge (me: Color) (graph: Graph.T) (uf: FastUnion.T) (edges: Edge.T seq) =
    let rnd = System.Random()
    let randomCenters = [|for i in 0..100 do yield rnd.Next (Graph.nVertices graph)|] |> Array.distinct
    let canTraverse edge = Graph.isClaimedBy me graph edge || not (Graph.isClaimed graph edge)
    let pathes = randomCenters |> Array.map (fun x-> Traversal.shortestPathWithPred graph x canTraverse)
    let sources = Graph.sources graph |> Seq.toArray
    let maxEdge, value = edges |>
                            Seq.map (fun x-> x, ScoreOneEdge sources uf pathes (Edge.ends x)) |>
                            Seq.maxBy (fun (a, b) -> b)
    maxEdge *)