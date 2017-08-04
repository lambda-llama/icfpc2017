module FastUnion

type T (graph: Graph.T) = 
    let maxVertId = graph.Verts |> Array.map (fun x -> x.Id) |> Array.max |> int
    let part = UnionFind.Partition maxVertId
    let unionarray : Union.T[] = [|
                    for i in 0..maxVertId do
                        for v in graph.Verts do
                        if int v.Id = i
                        then
                            if v.IsSource
                            then yield { Mines = [v.Id]; Sites = [] }
                            else yield { Mines = []; Sites = [v.Id] }
                        yield {Mines = []; Sites = []}|]

    member p.IsInSame v1 v2 =
        part.find(v1) = part.find(v2)
    
    member p.GetIncrement v1 v2 dist =
        if p.IsInSame v1 v2 
            then 0
            else
                Union.getScore unionarray.[part.find(v1)] unionarray.[part.find(v2)] dist

    member p.Unite v1 v2 =
        let oldP1 = part.find v1
        let oldP2 = part.find v2
        if part.union_by_rank (v1, v2)
            then
                let newP = part.find v1
                unionarray.[newP] <- Union.merge unionarray.[oldP1] unionarray.[oldP2]