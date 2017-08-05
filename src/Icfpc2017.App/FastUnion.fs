module FastUnion

open Graphs

type T (graph: Graph.T) =
    let maxVertId = graph.Verts |> Array.map (fun x -> x.Id) |> Array.max |> int |> fun x->x+1
    let part = UnionFind.Partition maxVertId
    let unionarray : Union.T[] = [|
                    for i in 0..maxVertId do
                        for v in graph.Verts do
                        if int v.Id = i
                        then yield Union.create v|]

    member p.TestGetUnion x = unionarray.[x]

    member p.IsInSame v1 v2 =
        part.find(v1) = part.find(v2)

    member p.GetIncrement(ver1: Graph.VertexId, ver2: Graph.VertexId, dist) =
        let v1 = int ver1
        let v2 = int ver2
        if p.IsInSame v1 v2
            then 0
            else
                Union.getScore unionarray.[part.find(v1)] unionarray.[part.find(v2)] dist

    member p.Unite (ver1: Graph.VertexId) (ver2: Graph.VertexId) =
        let v1 = int ver1
        let v2 = int ver2
        let oldP1 = part.find v1
        let oldP2 = part.find v2
        if part.union_by_rank (v1, v2)
            then
                let newP = part.find v1
                unionarray.[newP] <- Union.merge unionarray.[oldP1] unionarray.[oldP2]
