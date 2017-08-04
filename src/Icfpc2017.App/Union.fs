module Union

type T = {
    Sources: Graph.VertexId list
    Vertexes: Graph.VertexId list
}

let IsInUnion (union:T) (vertex:Graph.VertexId) = 
    union.Sources.exists (fun x -> x = int vertex) vertex

let getOneSideScore (sources: Graph.VertexId list) (vertexes: Graph.VertexId list) (dist: Map<int,int[]>) = 
    let mutable sum = 0 in
    for s in sources do
        for v in vertexes do
            let d = dist.[int s].[int v] in
            sum <- sum + d * d
    sum

let getScore union1 union2 dist =
    getOneSideScore union1.Sources union2.Vertexes dist + getOneSideScore union2.Sources union1.Vertexes dist

let merge (union1: T) (union2: T) = 
    { Sources = union1.Sources@union2.Sources; Vertexes = union1.Vertexes@union2.Vertexes }