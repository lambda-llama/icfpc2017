module GreadyStrategy

open Graphs
open Union

let getComponentVerts (graph: Graph.T) (part: UnionFind.Partition) (comp: int) =
    [|0..Graph.nVertices graph|] |>
    Array.filter (fun x-> part.find(x) = comp)

let greadyStrategy: Strategy.T =
    Strategy.stateless "gready" Map.empty (fun game ->
        let graph = game.Graph
        let uf = UnionFind.Partition (Graph.nVertices graph)
        let bfs = Traversal.shortestPaths graph
        for e in Graph.claimedBy graph game.Me do
            uf.union_by_rank (Edge.ends e)
        let comps = [|0..Graph.nVertices graph - 1|] |> Array.map (fun x -> uf.find(x)) |> Seq.distinct
        let unions = comps |>
                        Seq.map (fun comp->
                            (comp,{ Sources = getComponentVerts graph uf comp |>
                                                Array.filter (fun x -> Vertex.isSource (Graph.vertex graph x) );
                                    Sites = getComponentVerts graph uf comp |>
                                                Array.filter (fun x -> not (Vertex.isSource (Graph.vertex graph x)))})) |>
                        Map.ofSeq

        let getScore (edge: Edge.T) =
            if Graph.isClaimed graph edge
                then -1
                else 
                    let v1, v2 = Edge.ends edge
                    let p1 = uf.find(v1)
                    let p2 = uf.find(v2)
                    if uf.find(v1) = uf.find(v2)
                    then 0
                    else getScore (Map.find p1 unions) (Map.find p2 unions) bfs
        Graph.edges graph |> Array.maxBy getScore, game.StrategyState
    )