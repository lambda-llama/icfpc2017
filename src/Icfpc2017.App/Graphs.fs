module Graphs

open System
open System.Collections.Generic

open MessagePack
open Newtonsoft.Json

open Pervasives

type Color = ProtocolData.Color

module Vertex =
    [<Struct; MessagePackObject>]
    type T = {
        [<Key(0)>]
        Id: int
        [<Key(1)>]
        Coords: (float * float) option
    }

    type private S = int * (float * float) option

    type Converter () =
        inherit JsonConverter ()

        override x.CanConvert (typ: Type) = typ = typeof<T>

        override x.WriteJson (writer: JsonWriter, value: obj, serializer: JsonSerializer) =
            match value with
            | :? T as e -> serializer.Serialize (writer, (e.Id, e.Coords))
            | _ -> impossible

        override x.ReadJson (reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
            let (id, coords) = serializer.Deserialize<S> reader
            box {Id=id; Coords=coords}

    let create id isSource coords: T =
        {Id=(if isSource then -id else id); Coords=coords}

    let id {Id=id} = abs id
    let isSource {Id=id} = id < 0
    let coords {Coords=coords} = coords

module Edge =
    [<Struct; MessagePackObject>]
    type T = {
        [<Key(0)>]
        id: int
        [<Key(1)>]
        uv: int * int
    }

    type private S = int * (int * int)

    type Converter () =
        inherit JsonConverter ()

        override x.CanConvert (typ: Type) = typ = typeof<T>

        override x.WriteJson (writer: JsonWriter, value: obj, serializer: JsonSerializer) =
            match value with
            | :? T as e -> serializer.Serialize (writer, (e.id, e.uv))
            | _ -> impossible

        override x.ReadJson (reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
            let (id, uv) = serializer.Deserialize<S> reader
            box {id=id; uv=uv}

    (* Enforces an invariant that the first vertex ID is smaller. *)
    let create id (u, v) = {id=id; uv=(min u v, max u v)}

    let id {id=id} = id
    let ends {uv=uv} = uv

    let opposite {uv=(u, v)} w =
        if w = u
        then v
        else
            assert (w = v)
            u

    let contains {uv=(u, v) } w = u = w || v = w

(**
 * The Graph.
 *)
module Graph =
    [<Struct; MessagePackObject>]
    type T = {
        [<Key(0)>]
        Vertices: Vertex.T array
        [<Key(1)>]
        Sources: int array
        [<Key(2)>]
        Edges: Edge.T array
        [<Key(3)>]
        Colors: Map<int, Color>
        [<Key(4)>]
        AdjacentEdges: Edge.T array array
    }

    let private buildAdjacentEdges vertices edges =
        vertices
        |> Array.map (fun v -> edges |> Array.filter (fun e -> Edge.contains e (Vertex.id v)))

    type private S = Vertex.T array * int array * Edge.T array * Map<int, Color>

    type Converter () =
        inherit JsonConverter ()

        override x.CanConvert (typ: Type) = typ = typeof<T>

        override x.WriteJson (writer: JsonWriter, value: obj, serializer: JsonSerializer) =
            match value with
            | :? T as g ->
                let surrogate: S = (g.Vertices, g.Sources, g.Edges, g.Colors)
                serializer.Serialize(writer, surrogate)
            | _ -> impossible

        override x.ReadJson (reader: JsonReader, objectType: Type, existingValue: obj, serializer: JsonSerializer) =
            let (vertices, sources, edges, colors) = serializer.Deserialize<S> reader
            box {Vertices=vertices;
                 Sources=sources;
                 Edges=edges;
                 Colors=colors;
                 AdjacentEdges=buildAdjacentEdges vertices edges}

    (** Simplified [create] intended ONLY for test use. *)
    let testCreate nVertices sources uvs: T =
        let vertices = Array.init nVertices (fun vid ->
            Vertex.create vid (Array.contains vid sources) None)
        let edges = Array.mapi Edge.create uvs

        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         Colors=Map.empty;
         AdjacentEdges=buildAdjacentEdges vertices edges}

    let create vertices edges: T =
        let sources = vertices |> Array.choose (fun v ->
            if Vertex.isSource v then Some (Vertex.id v) else None)

        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         Colors=Map.empty;
         AdjacentEdges=buildAdjacentEdges vertices edges}

    let vertex {Vertices=vs} vid: Vertex.T = vs.[vid]
    let private vertices {Vertices=vs} = vs
    let private edges {Edges=es} = es

    let sources {Sources=sources}: int seq = Array.toSeq sources
    let sinks {Sources=sources; Vertices=vs}: int seq =
        Array.toSeq vs
        |> Seq.map Vertex.id
        |> Seq.filter (fun vid -> Array.contains vid sources)

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
