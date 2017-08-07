module FastUnion

open Graphs
open Union

type T = {
    partition: UnionFind.Partition;
    Unions: Map<int, Union.T>
}

type UnionWithEdge = {
    FastUnion: T
    P1: int
    P2: int
}

let createUnionWithEdge (fastUnion: T) (v1: int) (v2: int) =
    { FastUnion = fastUnion; P1 = fastUnion.partition.find(v1); P2 = fastUnion.partition.find(v2) }

let getUWEParent (uwe: UnionWithEdge) (v: int) =
    let originalParent = uwe.FastUnion.partition.find(v)
    if originalParent = uwe.P2
    then uwe.P1
    else originalParent

let getComponentVerts (graph: Graph.T) (part: UnionFind.Partition) (comp: int) =
    [|0..Graph.nVertices graph|] |>
    Array.filter (fun x-> part.find(x) = comp)

let create (graph: Graph.T) (me: Color) =
    let uf = UnionFind.Partition (Graph.nVertices graph)
    for e in Graph.claimedBy graph me do
        uf.union_by_rank (Edge.ends e) |> ignore
    let comps = {0..Graph.nVertices graph - 1} |> Seq.map uf.find |> Seq.distinct
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

let IsVirginNode (dsu: T) (v: int) =
    let un = getUnion dsu v
    un.Sources.Length = 0 && un.Sites.Length = 1