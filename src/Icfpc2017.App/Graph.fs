module Graph

type VertexId = uint32
type Color = uint32

type Edge = {
    // Invariant: first vertex is smaller,
    // see `normalizeEdgeEnds`.
    Ends : VertexId * VertexId;
    Color : Color option;
}

type T = {
    NVerts : VertexId;
    Edges : Edge list
}


let normalizeEdgeEnds (u, v) =
    (min u v, max u v)

let isUnclaimedEdge edge = Option.isNone edge.Color

let isEndPoint { Ends = (u, v) } vertex = vertex = u || vertex = v


let empty = {
    NVerts = 0u;
    Edges = [];
}

let create verts edges = 
    let newEdge e = { Ends = normalizeEdgeEnds e; Color = None} in 
    { NVerts = verts; Edges = List.map newEdge edges; }

let claimEdge graph color edge = 
    let claimed = normalizeEdgeEnds edge in
    let claim { Ends = ends; Color = c} =
        if ends = claimed 
        then { Ends = ends; Color = Some color }
        else { Ends = ends; Color = c }
    in
    { NVerts = graph.NVerts; Edges = List.map claim graph.Edges }

let private colors = [| 
    "red"; "blue"; "pink"; "yellow"; "cyan"; "dimgrey"; "green"; "indigo" 
|]

let toDot graph = 
    let renderNode id = sprintf "  %d [label=\"%d\"]" id id in 
    let renderEdge { Ends = (u, v); Color = c} = 
        let color: string = 
            match c with 
            | Some(idx) -> Array.get colors (int idx) 
            | None -> "black" 
        in sprintf "  %d -- %d [color=\"%s\"]" u v color
    in
    let mapJoin f xs = List.map f xs |> String.concat "\n" in 
    let nodes = [0u .. (graph.NVerts - 1u)] |> mapJoin renderNode in
    let edges = graph.Edges |> mapJoin renderEdge in 
    sprintf "graph {\n%s\n%s\n}" nodes edges