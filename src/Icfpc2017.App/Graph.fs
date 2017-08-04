module Graph

type VertexId = uint32
type Color = uint32 option

type Edge = {
    // Invariant: first vertex is sorted,
    // see `normalizeEdgeEnds`.
    Ends : VertexId * VertexId;
    Color : Color;
}

type T = {
    NVerts : VertexId;
    Edges : Edge list
}


let normalizeEdgeEnds (u, v) =
    (min u v, max u v)

let isUnclaimedEdge edge = Option.isNone edge.Color


let empty = {
    NVerts = 0u;
    Edges = [];
}

let claimEdge graph color edge = 
    let claimed = normalizeEdgeEnds edge in
    let claim { Ends = ends; Color = c} =
        if ends = claimed 
        then { Ends = ends; Color = Some color }
        else { Ends = ends; Color = c }
    in
    { NVerts = graph.NVerts; Edges = List.map claim graph.Edges }