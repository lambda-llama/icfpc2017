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
        {Id=(if isSource then -id - 1 else id); Coords=coords}

    let id {Id=id} = if id < 0 then -(id + 1) else id
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

    let defaultFilter e = true

    type T = private {
        CancelationToken: CancellationToken option
        Vertices: Vertex.T array
        Sources: int array
        Edges: Edge.T array
        EdgeFilter: Edge.T -> bool
        Colors: Map<int, Color * Color>
        OptionsLeft: int
        AdjacentEdges: Edge.T array array
    } with
        static member Read (r: BinaryReader): T =
            let vertices = r.ReadArray (fun _ -> Vertex.T.Read r)
            let sources = r.ReadArray (fun _ -> r.ReadInt32 ())
            let edges = r.ReadArray (fun _ -> Edge.T.Read r)
            let colors = r.ReadMap (fun _ ->
                (r.ReadInt32 (), (r.ReadInt32 (), r.ReadInt32 ())))
            let optionsLeft = r.ReadInt32()
            {Vertices=vertices; Sources=sources; Edges=edges;
             EdgeFilter=defaultFilter; Colors=colors; OptionsLeft=optionsLeft;
             AdjacentEdges=buildAdjacentEdges vertices edges;
             CancelationToken=None}

        member g.Write (w: BinaryWriter): unit =
            (* TODO: write only nVertices. *)
            w.WriteArray (g.Vertices, fun v -> v.Write w)
            w.WriteArray (g.Sources, w.Write)
            w.WriteArray (g.Edges, fun e -> e.Write w)
            w.WriteMap (g.Colors, fun eid (c1, c2) ->
                w.Write eid; w.Write c1; w.Write c2)
            w.Write g.OptionsLeft

    let withCancelationToken g t = { g with CancelationToken = Some t }

    (** Simplified [create] intended ONLY for test use. *)
    let testCreate nVertices sources uvs: T =
        let vertices = Array.init nVertices (fun vid ->
            Vertex.create vid (Array.contains vid sources) None)
        let edges = Array.mapi Edge.create uvs

        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         EdgeFilter=defaultFilter;
         Colors=Map.empty;
         OptionsLeft=0;
         AdjacentEdges=buildAdjacentEdges vertices edges;
         CancelationToken=None}

    let create vertices edges options: T =
        let sources = vertices |> Array.choose (fun v ->
            if Vertex.isSource v then Some (Vertex.id v) else None)

        {Vertices=vertices;
         Sources=sources;
         Edges=edges;
         EdgeFilter=defaultFilter;
         Colors=Map.empty;
         OptionsLeft=if options then sources.Length else 0;
         AdjacentEdges=buildAdjacentEdges vertices edges;
         CancelationToken=None}

    let vertex {Vertices=vs} vid: Vertex.T = vs.[vid]
    let private vertices {Vertices=vs} = vs
    let private edges {Edges=es} = Array.toSeq es

    let sources {Sources=sources}: int seq = Array.toSeq sources
    let sinks {Sources=sources; Vertices=vs}: int seq =
        Array.toSeq vs
        |> Seq.map Vertex.id
        |> Seq.filter (fun vid -> Array.contains vid sources |> not)

    let nVertices = vertices >> Array.length
    let nEdges = edges >> Seq.length

    let edgeId g uv =
        (* XXX normalize ends. *)
        let uv = Edge.create 0 uv |> Edge.ends
        Seq.find (fun e -> Edge.ends e = uv) (edges g) |> Edge.id

    let edgeColors {Colors=cs; EdgeFilter=ef} edge =
        if ef edge
        then cs.TryFind (Edge.id edge)
        else None

    let adjacentEdges {AdjacentEdges=adj; EdgeFilter=ef} vid: Edge.T seq =
        Array.toSeq adj.[vid] |> Seq.filter ef

    let adjacent g vid =
        adjacentEdges g vid |> Seq.map (fun e -> Edge.opposite e vid)

    let isClaimed {Colors=cs; EdgeFilter=ef} edge: bool =
        ef edge && cs.ContainsKey (Edge.id edge)

    let isClaimedBy punter g edge: bool =
        match edgeColors g edge with
        | Some (c1, c2) -> c1 = punter || c2 = punter
        | None -> false

    let private isBought g edge =
        match edgeColors g edge with
            | Some (c1, c2) -> c1 <> c2
            | None -> false

    let isOptionFor punter g edge: bool =
        isClaimed g edge && not (isBought g edge || isClaimedBy punter g edge)

    (* XXX this is *I* can buy. *)
    let canBuy g edge: bool =
        g.EdgeFilter edge &&
        g.OptionsLeft > 0 &&
        isClaimed g edge && not (isBought g edge)

    let claimOptionEdge ({Colors=cs; Edges=es} as g) punter me eid: T =
        g.CancelationToken
            |> Option.map (fun t -> t.ThrowIfCancellationRequested ())
            |> ignore
        if isOptionFor punter g es.[eid]
        then
            let (other, _) = Map.find eid cs
            {g with Colors=Map.add eid (other, punter) cs;
                    OptionsLeft=g.OptionsLeft - (if punter = me then 1 else 0)}
        else
            {g with Colors=Map.add eid (punter, punter) cs}

    let claimEdge g punter eid = claimOptionEdge g punter punter eid

    let claimedBy g punter: Edge.T seq =
        edges g |> Seq.filter (isClaimedBy punter g)

    (* XXX use only for [Game.Me]. *)
    let claimedByOrCanBuy g punter: Edge.T seq =
        edges g |> Seq.filter (fun edge ->
            isClaimedBy punter g edge || canBuy g edge)

    let unclaimed g: Edge.T seq =
        edges g |> Seq.filter (isClaimed g >> not)

    let unclaimedOrCanBy g: Edge.T seq =
        edges g |> Seq.filter (fun edge ->
            not (isClaimed g edge) || canBuy g edge)

    (** Focus on a subgraph of a specific punter. *)
    let subgraph (g: T) (punter: Color): T =
        {g with EdgeFilter=isClaimedBy punter g}

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
                (* TODO: multiple colors?.. *)
                match edgeColors graph e with
                | Some(c1, c2) when c1 = we || c2 = we -> ("red", 3)
                | Some(c1, c2) -> (Array.get colors (int c1), 3)
                | None -> ("black", 1)
            in sprintf "  %d -- %d [color=\"%s\", penwidth=\"%d\"];" u v color width
        in
        let nodes = vertices graph |> Seq.map renderVertex |> String.concat "\n"
        let edges = edges graph |> Seq.map renderEdge |> String.concat "\n"
        sprintf "graph {\n%s\n%s\n}" nodes edges

module Traversal =
    let connectedComponents (graph: Graph.T): Graph.T seq =
        let nVertices = Graph.nVertices graph
        let components = Array.create nVertices -1
        let explore source =
            let work = new Stack<int>()
            let pushIfNeeded vid =
                if components.[vid] = -1
                then
                    components.[vid] <- source
                    work.Push vid

            pushIfNeeded source
            while work.Count <> 0 do
                let current = work.Pop ()
                for next in Graph.adjacent graph current do
                    pushIfNeeded next

        for source in Graph.sources graph do
            explore source

        let belongsTo edge c =
            let (u, v) = Edge.ends edge
            components.[u] = c  (* auto-true for [v]. *)
        let cutout target =
            let vertices =
                [|for vid in 0..nVertices - 1 do
                  if components.[vid] = target
                  then yield Graph.vertex graph vid|]
            if vertices.Length = 0
            then None
            else
                (* HACK: this assumes no edges have been claimed. *)
                let edges =
                    [|for edge in Graph.unclaimed graph do
                      if belongsTo edge target then yield edge|]
                Some (Graph.create vertices edges false)
        (* XXX by designed this would filter out chunks of the graph
               disconnected from the [sources]. *)
        Graph.sources graph |> Seq.choose cutout

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
