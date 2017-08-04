module Graph

type VertexId = uint32
type Color = uint32 option

type Edge = {
    ends : VertexId * VertexId;
    color : Color;
}


type T = {
    NVerts : VertexId;
    Edges : Edge list
}

let empty = {
    NVerts = 0u;
    Edges = [];
}