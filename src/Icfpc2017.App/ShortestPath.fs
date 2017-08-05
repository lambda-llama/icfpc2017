module ShortestPath

open System.Collections.Generic

let BFS (graph: Graph.T) (mine: Graph.VertexId) (distances: int[]) =
     let q = new Queue<Graph.VertexId * int>() in     
     distances.[mine] <- 0;
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
                if distances.[nextVertex] = -1
                then begin
                    distances.[nextVertex] <- d + 1;
                    q.Enqueue (nextVertex, d + 1) 
                end
            end

let Compute (graph: Graph.T) = 
    let maxVertId = graph.Verts |> Array.map (fun x -> x.Id) |> Array.max |> int in
    graph.Verts |> Array.filter (fun v -> v.IsSource)
        |> Array.map (fun source ->
            let distances = Array.create maxVertId -1 in
            BFS graph source.Id distances
            (source.Id, distances))
        |> Map.ofArray