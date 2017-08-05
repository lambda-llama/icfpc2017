module Graphs

open System.Collections.Generic

type Color = int

type Vertex = {
    IsSource: bool;
    Coords: (float * float) option
}

module Edge =
    [<CustomEqualityAttribute; NoComparisonAttribute>]
    type T =
        private {
            Ends: int * int
            Color: Color option
        }

        override x.GetHashCode() = hash x.Ends
        override x.Equals(other) =
            match other with
            | :? T as y -> x.Ends = y.Ends
            | _ -> false

    (* Enforces an invariant that the first vertex ID is smaller. *)
    let create (u, v) = {Ends=(min u v, max u v); Color=None}

    let ends {Ends=ends} = ends

    let opposite {Ends=(u, v)} w =
        if w = u
        then v
        else
            assert (w = v)
            u

    let contains {Ends=(u, v)} w = u = w || v = w

    let isClaimed {Color=optC} = Option.isSome optC
    let isClaimedBy {Color=optC} punter =
        Option.map ((=) punter) optC |> Option.defaultValue false

    let claim edge punter = {edge with Color=Some punter}

(**
 * The Graph.
 *)
module Graph =
    type T = private {
        NVertices: int
        Sources: int array
        Edges: Edge.T array
    }

    let create nVertices sources uvs: T =       
        {NVertices=nVertices; 
         Sources=sources;
         Edges=Array.map Edge.create uvs}

    let vertices {NVertices=nVertices} = [0..nVertices - 1]
    let sources {Sources=sources} = sources
    let edges {Edges=es} = es

    let nVertices {NVertices=nVertices} = nVertices
    let nEdges = edges >> Array.length

    let adjacent {Edges=es} vid =
        es
        |> Array.filter (fun e -> Edge.contains e vid)
        |> Array.map (fun e -> Edge.opposite e vid)

    let claimEdge {Edges=es} eid punter =
        es.[eid] <- Edge.claim es.[eid] punter   

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
