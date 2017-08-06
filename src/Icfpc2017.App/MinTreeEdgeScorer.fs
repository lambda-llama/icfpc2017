module MinTreeEdgeScorer

open Graphs

let ScoreEdges (me: Color) (graph: Graph.T) (edges: Edge.T list) =
    let rnd = System.Random()
    let uf = UnionFind.Partition (Graph.nVertices graph)
    Graph.edges graph |> Seq.
        if Graph.isClaimedBy me graph e
            then uf.union_by_rank(e.Ends)
    let randomCenters = [|for i in 0..100 do yield rnd.Next (Graph.nVertices graph)|] |> Array.distinct
    randomCenters |>
    Array.map Traversal.shortestPathWithPred graph (fun e -> Graph.isClaimedBy graph me || not Graph.isClaimed graph)
