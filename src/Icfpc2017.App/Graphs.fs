module Graphs

open System.Collections.Generic

type Color = int

(* TODO: make private *)
type Vertex = {
    IsSource: bool;
    Coords: (float * float) option
}

module Edge =
    type T = private Edge of (int * int)

    (* Enforces an invariant that the first vertex ID is smaller. *)
    let create uv = Edge uv

    let ends (Edge uv) = uv

    let opposite (Edge (u, v)) w =
        if w = u
        then v
        else
            assert (w = v)
            u

    let contains (Edge (u, v)) w = u = w || v = w    

(**
 * The Graph.
 *)
module Graph =
    type T = private {
        NVertices: int
        Vertices: Vertex array
        Sources: int array
        Edges: Edge.T array
        Colors: Map<int, Color>
    }

    let create nVertices sources uvs: T =
        let vertices =
            [0..nVertices - 1]
            |> List.map (fun v -> sources |> Array.contains v)
            |> List.map (fun s -> { IsSource = s; Coords = None })
            |> List.toArray
            
        {NVertices=nVertices;
         Vertices=vertices;
         Sources=sources;
         Edges=Array.map Edge.create uvs;
         Colors=Map.empty}

    let vertices {Vertices=vertices} = vertices
    let sources {Sources=sources} = sources
    let edges {Edges=es} = es

    let nVertices {NVertices=nVertices} = nVertices
    let nEdges = edges >> Array.length

    let adjacent {Edges=es} vid =
        es
        |> Array.filter (fun e -> Edge.contains e vid)
        |> Array.map (fun e -> Edge.opposite e vid)

    let claimEdge ({Colors=cs} as g) punter eid: T =
        {g with Colors=Map.add eid punter cs}

module Traversal =
    (** Computes the shortest paths from [source] to all other vertices. *)
    let shortestPath (graph: Graph.T) (source: int): int array =
        let distances = Array.zeroCreate (Graph.nVertices graph) in
        let seen = Array.create (Graph.nVertices graph) false in
        let q = new Queue<int>() in
        distances.[source] <- 0
        q.Enqueue source
        while q.Count <> 0 do
            let current = q.Dequeue () in
            seen.[current] <- true
            for next in Graph.adjacent graph current do                
                if not seen.[next]
                then
                    distances.[next] <- distances.[current] + 1
                    q.Enqueue next

        distances
