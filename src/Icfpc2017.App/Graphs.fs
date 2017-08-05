module Graphs

open System.Collections.Generic

type Color = ProtocolData.Color

module Vertex =
    type T = private {
        Id: int
        IsSource: bool
        Coords: (float * float) option
    }

    let create id isSource coords: T = {Id=id; IsSource=isSource; Coords=coords}

    let id {Id=id} = id
    let isSource {IsSource=isSource} = isSource
    let coords {Coords=coords} = coords

module Edge =
    type T = private {
        id: int
        uv: int * int
    }

    (* Enforces an invariant that the first vertex ID is smaller. *)
    let create id (u, v) = {id=id; uv=(min u v, max u v)}

    let id { id=id } = id
    let ends { uv=uv } = uv

    let opposite { uv=(u, v) } w =
        if w = u
        then v
        else
            assert (w = v)
            u

    let contains { uv=(u, v) } w = u = w || v = w

(**
 * The Graph.
 *)
module Graph =
    type T = private {
        Vertices: Vertex.T array
        Sources: int array
        Edges: Edge.T array
        Colors: Map<int, Color>
    }

    (** Simplified [create] intended ONLY for test use. *)
    let testCreate nVertices sources uvs: T =
        let vertices = Array.init nVertices (fun vid ->
            Vertex.create vid (Array.contains vid sources) None)
        {Vertices=vertices;
         Sources=sources;
         Edges=Array.mapi Edge.create uvs;
         Colors=Map.empty}

    let create vertices edges: T =
        let sources = vertices |> Array.choose (fun v ->
            if Vertex.isSource v then Some (Vertex.id v) else None)
        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         Colors=Map.empty}

    let vertices {Vertices=vertices} = vertices
    let sources {Sources=sources} = sources
    let edges {Edges=es} = es

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
        {g with Edges=subEdges; Colors=subColors}

    let adjacentEdges {Edges=es} vid =
        Array.toSeq es |> Seq.filter (fun e -> Edge.contains e vid)

    let adjacent g vid =
        adjacentEdges g vid |> Seq.map (fun e -> Edge.opposite e vid)

    let isClaimed {Colors=cs} edge: bool = cs.ContainsKey (Edge.id edge)

    let isClaimedBy punter g edge: bool =
        match edgeColor g edge with
        | Some color -> color = punter
        | None -> false

    let claimEdge ({Colors=cs} as g) punter eid: T =
        {g with Colors=Map.add eid punter cs}

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
    let shortestPath (graph: Graph.T) (source: int): int array =
        let distances = Array.create (Graph.nVertices graph) -1 in
        let work = new Queue<int>() in

        let enqueueIfNeed x dist =
            if distances.[x] = -1 then
                work.Enqueue x
                distances.[x] <- dist

        enqueueIfNeed source 0

        while work.Count <> 0 do
            let current = work.Dequeue () in
            for next in Graph.adjacent graph current do
                assert (distances.[current] <> -1)
                enqueueIfNeed next (distances.[current] + 1)

        distances

    let shortestPaths (graph: Graph.T): Map<int, int array> =
        Graph.sources graph
        |> Array.map (fun v -> (v, shortestPath graph v))
        |> Map.ofArray
// bruteForce1 2424
// bruteForce3 3313
// minimax 2603
