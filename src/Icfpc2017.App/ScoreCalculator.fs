type Union = {
    Sources: Graph.VertexId list;
    Vertexes: Graph.VertexId list;
}

let getOneSideScore sources vertexes dist = 
    let sum = 0 in
    for s in sources do
        for v in vertexes do
            sum <- sum + dist.[s, v] * dist.[s, v] in
    sum

let getScore union1 union2 dist =
    getOneSideScore union1.Sources union2.Vertexes dist + getOneSideScore union2.Sources union1.Vertexes dist

let unify union1 union2 = 
    { Sources = union1.Source@union2.Source; Vertex = union1.Vertexes@union2.Vertex }