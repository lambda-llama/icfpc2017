module Union

type T = {
    Mines: Graph.VertexId list
    Sites: Graph.VertexId list
}

let getOneSideScore (mines: Graph.VertexId list) (sites: Graph.VertexId list) (dist: Map<Graph.VertexId, int[]>) = 
    let mutable sum = 0 in
    for m in mines do
        for s in sites do
            let d = dist.[m].[int s] in
            sum <- sum + d * d
    sum

let getScore union1 union2 dist =
    getOneSideScore union1.Mines union2.Sites dist + getOneSideScore union2.Mines union1.Sites dist

let merge (union1: T) (union2: T) = 
    { Mines = union1.Mines@union2.Mines; Sites = union1.Sites@union2.Sites }