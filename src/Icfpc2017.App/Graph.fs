module Graph

type VertexId = uint32
type Color = uint32 option

type Edge = {
    Ends : VertexId * VertexId;
    Color : Color;
}

let isUnclaimedEdge edge = Option.isNone edge.Color


type T = {
    NVerts : VertexId;
    Edges : Edge list
}

let empty = {
    NVerts = 0u;
    Edges = [];
}

