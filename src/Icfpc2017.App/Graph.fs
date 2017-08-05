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

