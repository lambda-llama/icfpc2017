module ShortestPath

open System.Collections.Generic

let BFS (graph: Graph.T) (mine: Graph.VertexId) (distances: int[,]) =
     let q = new Queue<Graph.VertexId * int>() in
     q.Enqueue (mine, 0);
     while q.Count <> 0 do
        let (vertex, d) = q.Dequeue () in
        for edge in graph.Edges do
            let (ver1, ver2) = edge.Ends in
            if ver1 = vertex || ver2 = vertex 
            then begin
                let nextVertex =
                    if ver1 = vertex then ver2
                    else ver1 
                in
                if distances.[int mine, int nextVertex] = -1
                then begin
                    distances.[int mine, int nextVertex] <- d + 1;
                    q.Enqueue (nextVertex, d + 1) 
                end
            end

let Compute (graph: Graph.T) (mines: Graph.VertexId list)= 
    let res = Array2D.init mines.Length (int graph.NVerts) (fun a b -> -1) in
    for mine in mines do
        BFS graph mine res
