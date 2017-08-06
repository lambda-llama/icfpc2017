module Graphs

open System
open System.IO
open System.Collections.Generic
open System.Threading


open Pervasives

type Color = ProtocolData.Color

module Vertex =
    [<Struct>]
    type T = private {
        Id: int
        Coords: (float * float) option
    } with
        static member Read(r: BinaryReader): T =
            {Id=r.ReadInt32 (); Coords=None}

        member v.Write(w: BinaryWriter): unit =
            w.Write v.Id

    let create id isSource coords: T =
        {Id=(if isSource then -id else id); Coords=coords}

    let id {Id=id} = abs id
    let isSource {Id=id} = id < 0
    let coords {Coords=coords} = coords

module Edge =
    [<Struct>]
    type T = private {
        Id: int
        U: int
        V: int
    } with
        static member Read(r: BinaryReader): T =
            let id = r.ReadInt32 ()
            let u = r.ReadInt32 ()
            let v = r.ReadInt32 ()
            {Id=id; U=u; V=v}

        member e.Write(w: BinaryWriter): unit =
            w.Write e.Id
            w.Write e.U
            w.Write e.V

    (* Enforces an invariant that the first vertex ID is smaller. *)
    let create id (u, v) = {Id=id; U=min u v; V=max u v}

    let id {Id=id} = id
    let ends {U=u; V=v} = (u, v)

    let opposite {U=u; V=v} w =
        if w = u
        then v
        else
            assert (w = v)
            u

    let contains {U=u; V=v} w = u = w || v = w

(**
 * The Graph.
 *)
module Graph =
    let private buildAdjacentEdges vertices edges =
        vertices
        |> Array.map (fun v -> edges |> Array.filter (fun e -> Edge.contains e (Vertex.id v)))

    type T = private {
        CancelationToken: CancellationToken option
        Vertices: Vertex.T array
        Sources: int array
        Edges: Edge.T array
        Colors: Map<int, Color>
        AdjacentEdges: Edge.T array array
    } with
        static member Read (r: BinaryReader): T =
            let vertices = r.ReadArray (fun _ -> Vertex.T.Read r)
            let sources = r.ReadArray (fun _ -> r.ReadInt32 ())
            let edges = r.ReadArray (fun _ -> Edge.T.Read r)
            let colors = r.ReadMap (fun _ -> (r.ReadInt32 (), r.ReadInt32 ()))
            {Vertices=vertices; Sources=sources; Edges=edges;
             Colors=colors;
             AdjacentEdges=buildAdjacentEdges vertices edges;
             CancelationToken=None}

        member g.Write (w: BinaryWriter): unit =
            (* TODO: write only nVertices. *)
            w.WriteArray (g.Vertices, fun v -> v.Write w)
            w.WriteArray (g.Sources, w.Write)
            w.WriteArray (g.Edges, fun e -> e.Write w)
            w.WriteMap (g.Colors, fun k v -> w.Write k; w.Write v)

    let withCancelationToken g t = { g with CancelationToken = Some t }

    (** Simplified [create] intended ONLY for test use. *)
    let testCreate nVertices sources uvs: T =
        let vertices = Array.init nVertices (fun vid ->
            Vertex.create vid (Array.contains vid sources) None)
        let edges = Array.mapi Edge.create uvs

        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         Colors=Map.empty;
         AdjacentEdges=buildAdjacentEdges vertices edges;
         CancelationToken=None}

    let create vertices edges: T =
        let sources = vertices |> Array.choose (fun v ->
            if Vertex.isSource v then Some (Vertex.id v) else None)

        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         Colors=Map.empty;
         AdjacentEdges=buildAdjacentEdges vertices edges;
         CancelationToken=None}

    let vertex {Vertices=vs} vid: Vertex.T = vs.[vid]
    let private vertices {Vertices=vs} = vs
    let private edges {Edges=es} = es

    let sources {Sources=sources}: int seq = Array.toSeq sources
    let sinks {Sources=sources; Vertices=vs}: int seq =
        Array.toSeq vs
        |> Seq.map Vertex.id
        |> Seq.filter (fun vid -> Array.contains vid sources |> not)

    let nVertices = vertices >> Array.length
    let nEdges = edges >> Array.length

    let edgeId {Edges=es} uv =
        (* XXX normalize ends. *)
        let uv = Edge.create 0 uv |> Edge.ends
        Array.find (fun e -> Edge.ends e = uv) es |> Edge.id

    let edgeColor {Colors=cs} e = cs.TryFind (Edge.id e)

    (** Focus on a subgraph of a specific color. *)
    let subgraph (g : T) (color: Color): T =
        (* TODO: ideally just filter in [[adjacent]]. *)
        let subColors = Map.filter (fun _ -> (=) color) g.Colors
        let subEdges =
            g.Edges
            |> Array.filter (fun edge -> Map.containsKey (Edge.id edge) subColors)
        {g with Edges=subEdges; Colors=subColors; AdjacentEdges=buildAdjacentEdges (vertices g) subEdges}

    let adjacentEdges {AdjacentEdges=adj} vid: Edge.T seq = Array.toSeq adj.[vid]

    let adjacent g vid =
        adjacentEdges g vid |> Seq.map (fun e -> Edge.opposite e vid)

    let isClaimed {Colors=cs} edge: bool = cs.ContainsKey (Edge.id edge)

    let isClaimedBy punter g edge: bool =
        match edgeColor g edge with
        | Some color -> color = punter
        | None -> false

    let claimEdge ({Colors=cs} as g) punter eid: T =
        let _ = g.CancelationToken |> Option.map (fun t -> t.ThrowIfCancellationRequested())
        {g with Colors=Map.add eid punter cs}

    let claimedBy ({Edges=es} as g) punter: Edge.T seq =
        Array.toSeq es |> Seq.filter (isClaimedBy punter g)

    let unclaimed ({Edges=es} as g): Edge.T seq =
        Array.toSeq es |> Seq.filter (isClaimed g >> not)

    let private colors = [|
        "blue"; "green"; "yellow"; "cyan"; "dimgrey"; "margenta"; "indigo"; "pink";
        "black"; "black"; "black"; "black"; "black"; "black"; "black"; "black";
        "black"; "black"; "black"; "black"; "black"; "black"; "black"; "black";
        "black"; "black"; "black"; "black"; "black"; "black"; "black"; "black";
        "black"; "black"; "black"; "black"; "black"; "black"; "black"; "black";
    |]

    let private makeScale (xs: float array): (float -> float) =
        let spread = max ((Array.max xs - Array.min xs) / 2.0) 1.0
        let mean = Array.sum xs / float (Array.length xs)
        fun t -> (t - mean) / spread

    let toDot we graph =
        let aux f = vertices graph |> Array.map (Vertex.coords >> Option.map f >> Option.defaultValue 1.0)
        let scaleX = makeScale (aux (fun (x, _) -> x))
        let scaleY = makeScale (aux (fun (_, y) -> y))
        let renderVertex v =
            let id = Vertex.id v
            let shape = if Vertex.isSource v then "square" else "circle" in
            let position =
                match Vertex.coords v with
                | None -> ""
                | Some((x, y)) -> sprintf ", pos=\"%f,%f!\"" (scaleX x) (- (scaleY y))
            in
            sprintf "  %d [label=\"%d\", shape=\"%s\"%s];" id id shape position
        in
        let renderEdge (e: Edge.T) =
            let (u, v) = Edge.ends e
            let (color: string, width: int) =
                match edgeColor graph e with
                | Some(idx) when idx = we -> ("red", 3)
                | Some(idx) -> (Array.get colors (int idx), 3)
                | None -> ("black", 1)
            in sprintf "  %d -- %d [color=\"%s\", penwidth=\"%d\"];" u v color width
        in
        let nodes = vertices graph |> Array.map renderVertex |> String.concat "\n"
        let edges = edges graph |> Array.map renderEdge |> String.concat "\n"
        sprintf "graph {\n%s\n%s\n}" nodes edges

module Traversal =
    (** Computes the shortest paths from [source] to all other vertices. *)
    let shortestPathWithPred (graph: Graph.T) (source: int) (pred: Edge.T -> bool): int array =
        let distances = Array.create (Graph.nVertices graph) -1 in
        let work = new Queue<int>() in

        let enqueueIfNeed x dist =
            if distances.[x] = -1 then
                work.Enqueue x
                distances.[x] <- dist

        enqueueIfNeed source 0

        while work.Count <> 0 do
            let current = work.Dequeue () in
            assert (distances.[current] <> -1)
            for next in Graph.adjacentEdges graph current |> Seq.filter pred |> Seq.map (fun e -> Edge.opposite e current) do
                enqueueIfNeed next (distances.[current] + 1)

        distances
    let shortestPath (graph: Graph.T) (vertex: int): int array =
        shortestPathWithPred graph vertex (fun e -> true)

    let shortestPaths (graph: Graph.T): Map<int, int array> =
        Graph.sources graph
        |> Seq.map (fun v -> (v, shortestPath graph v))
        |> Map.ofSeq
