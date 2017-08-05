module Graph

type VertexId = int
type Color = int

type Edge = {
    // Invariant: first vertex is smaller,
    // see `normalizeEdgeEnds`.
    Ends : VertexId * VertexId;
    Color : Color option;
}

type Vertex = {
    Id: VertexId;
    IsSource: bool;
    Coords: (float * float) option
}

type T = {
    Verts : Vertex array
    Edges : Edge list
}

let sanityCheck graph = 
    let maxId = graph.Verts |> Array.map (fun v -> v.Id) |> Array.max
    for {Ends = (u, v)} in graph.Edges do
        assert (u <= maxId && v <= maxId)


let normalizeEdgeEnds (u, v) =
    (min u v, max u v)

let isUnclaimedEdge edge = Option.isNone edge.Color

let unclaimedEdges graph = List.filter isUnclaimedEdge graph.Edges

let isEndPoint { Ends = (u, v) } vertex = vertex = u || vertex = v

let isSource { Verts = verts } vertex = 
    verts
    |> Array.tryFind (fun { Id = id; IsSource = isSource } -> id = vertex && isSource)
    |> Option.isSome

let outEdges (graph: T) (vertex: VertexId): Edge list =
    graph.Edges
    |> List.filter (fun e -> isEndPoint e vertex)

let sources (graph: T): VertexId array =
    graph.Verts |> Array.toSeq 
        |> Seq.filter (fun {IsSource=isSource} -> isSource)
        |> Seq.map (fun {Id=id} -> id)
        |> Array.ofSeq

let verticies (graph: T): VertexId seq = 
    graph.Verts
    |> Array.toSeq
    |> Seq.map (fun {Id = id} -> id)

let create verts edges =
    let newEdge e = { Ends = normalizeEdgeEnds e; Color = None} in
    { Verts = verts; Edges = List.map newEdge edges; }

let claimEdge graph color edge =
    let claimed = normalizeEdgeEnds edge in
    let claim { Ends = ends; Color = c} =
        if ends = claimed
        then { Ends = ends; Color = Some color }
        else { Ends = ends; Color = c }
    in
    { graph with Edges = List.map claim graph.Edges }

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
    let aux f = graph.Verts |> Array.map (fun v -> v.Coords |> Option.map f |> Option.defaultValue 1.0)
    let scaleX = makeScale (aux (fun (x, _) -> x))
    let scaleY = makeScale (aux (fun (_, y) -> y))
    let renderVertex {Id = id; IsSource = isSource; Coords = coords } =
        let shape = if isSource then "square" else "circle" in
        let position = 
            match coords with
            | None -> ""
            | Some((x, y)) -> sprintf ", pos=\"%f,%f!\"" (scaleX x) (- (scaleY y))
        in
        sprintf "  %d [label=\"%d\", shape=\"%s\"%s];" id id shape position
    in
    let renderEdge { Ends = (u, v); Color = c} =
        let (color: string, width: int) =
            match c with
            | Some(idx) when idx = we -> ("red", 3)
            | Some(idx) -> (Array.get colors (int idx), 3)
            | None -> ("black", 1)
        in sprintf "  %d -- %d [color=\"%s\", penwidth=\"%d\"];" u v color width
    in
    let nodes = graph.Verts |> Array.map renderVertex |> String.concat "\n"
    let edges = graph.Edges |> List.map renderEdge |> String.concat "\n"
    sprintf "graph {\n%s\n%s\n}" nodes edges