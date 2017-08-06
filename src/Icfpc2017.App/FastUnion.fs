module FastUnion

open Graphs
open Union

type T = {
    partition: UnionFind.Partition;
    Unions: Map<int, Union.T>
}

let getComponentVerts (graph: Graph.T) (part: UnionFind.Partition) (comp: int) =
    [|0..Graph.nVertices graph|] |>
    Array.filter (fun x-> part.find(x) = comp)

let create (graph: Graph.T) (me: Color)=
    let uf = UnionFind.Partition (Graph.nVertices graph)
    for e in Graph.claimedBy graph me do
        uf.union_by_rank (Edge.ends e)
    let comps = [|0..Graph.nVertices graph - 1|] |> Array.map (fun x -> uf.find(x)) |> Seq.distinct
    let unions = comps |>
                    Seq.map (fun comp->
                        (comp,{ Sources = getComponentVerts graph uf comp |>
                                            Array.filter (fun x -> Vertex.isSource (Graph.vertex graph x) );
                                Sites = getComponentVerts graph uf comp |>
                                            Array.filter (fun x -> not (Vertex.isSource (Graph.vertex graph x)))})) |>
                    Map.ofSeq
    { partition = uf; Unions = unions }

let getUnion (uf: T) (key: int)=
    let p = uf.partition.find(key)
    Map.find p uf.Unions

let IsSameComponent (uf: T) (u: int) (v: int)=
    uf.partition.find(u) = uf.partition.find(v)