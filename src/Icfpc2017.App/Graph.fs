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


let normalizeEdgeEnds (u, v) =
    (min u v, max u v)

let isUnclaimedEdge edge = Option.isNone edge.Color

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
    "blue"; "pink"; "yellow"; "cyan"; "dimgrey"; "green"; "indigo"; "margenta"
|]

let toDot we graph =
    let renderVertex {Id = id; IsSource = isSource; Coords = coords } =
        let shape = if isSource then "square" else "circle" in
        let position = 
            match coords with
            | None -> ""
            | Some((x, y)) -> sprintf ", pos=\"%f,%f!\"" x (-y)
        in
        sprintf "  %d [label=\"%d\", shape=\"%s\"%s];" id id shape position
    in
    let renderEdge { Ends = (u, v); Color = c} =
        let color: string =
            match c with
            | Some(idx) when idx = we -> "red"
            | Some(idx) -> Array.get colors (int idx)
            | None -> "black"
        in sprintf "  %d -- %d [color=\"%s\"];" u v color
    in
    let nodes = graph.Verts |> Array.map renderVertex |> String.concat "\n"
    let edges = graph.Edges |> List.map renderEdge |> String.concat "\n"
    sprintf "graph {\n%s\n%s\n}" nodes edges