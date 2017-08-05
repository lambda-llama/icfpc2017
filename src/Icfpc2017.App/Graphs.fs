module Graphs

open System.Collections.Generic

type VertexId = V of int
type Color = int

(* XXX [Id] should not be used internally. All graph methods assume
       that vertices are number 0..|V|-1. *)
type Vertex = {
    Id: VertexId;
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
        Vertices: Vertex array
        Resolver: Map<VertexId, int>
        Sources: int array
        Edges: Edge.T array
    }

    let create vertices uvs: T =
        let nVertices = Array.length vertices in
        let sources =
            {0..nVertices - 1}
            |> Seq.filter (fun vi -> vertices.[vi].IsSource)
            |> Seq.toArray
        in

        (* Mapping from external to internal IDs. *)
        let resolver =
            Seq.mapi (fun vid {Id=id} -> (id, vid)) vertices |> Map.ofSeq
        in

        {Vertices=vertices; Resolver=resolver;
         Sources=sources;
         Edges=Array.map (fun (u, v) ->
             Edge.create (resolver.[u], resolver.[v])) uvs}

    let vertices {Vertices=vs} = vs
    let sources {Sources=sources} = sources
    let edges {Edges=es} = es

    let nVertices = vertices >> Array.length
    let nEdges = edges >> Array.length

    let incident {Edges=es} vid =
        Array.filter (fun e -> Edge.contains e vid) es

    let claimEdge {Edges=es} eid punter =
        es.[eid] <- Edge.claim es.[eid] punter

    let vidToExternal {Vertices=vs} vid: VertexId = vs.[vid].Id
    let externalToVid {Resolver=resolver} id: int = resolver.[id]

    let eidToExternal ({Edges=es} as g) eid: (VertexId * VertexId) =
        let (u, v) = Edge.ends es.[eid] in
        (vidToExternal g u, vidToExternal g v)
    let externalToEid ({Edges=es} as g) (u, v): int =
        let e = Edge.create (externalToVid g u, externalToVid g v) in
        Array.findIndex ((=) e) es


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
            for edge in Graph.incident graph current do
                let next = Edge.opposite edge current in
                if not seen.[next]
                then
                    distances.[next] <- distances.[current] + 1
                    q.Enqueue next

        distances
