module Game

(* Mapping from external to internal IDs. *)
type Index<'a when 'a : comparison> = {
    eToI: Map<'a, int>
    iToE: 'a array
} with 
    static member create (iToE: 'a array): 'a Index = 
        let eToI = iToE 
                   |> Array.mapi (fun i item -> (item, i)) 
                   |> Map.ofArray
        {eToI=eToI; iToE=iToE}

    member x.i (key: 'a) = x.eToI.[key]
    member x.e (key: int) = x.iToE.[key]  

type VertexId = int

type State = {
    Graph: Graph.T
    Graph2: Graphs.Graph.T
    EIndex: (VertexId * VertexId) Index        
    Me: Graph.Color
    BFSDist: Map<Graph.VertexId, int[]>
    Union: FastUnion.T
    NumPlayers: int
}

let applyClaim state (claim: ProtocolData.Claim) = 
    let edge = (claim.source, claim.target)
    let eid = state.EIndex.i(edge)
    if claim.punter = state.Me
    then
        let _ = state.Union.Unite claim.source, claim.target 
        {state with 
           Graph = Graph.claimEdge state.Graph claim.punter edge
           Graph2 = Graphs.Graph.claimEdge state.Graph2 claim.punter eid}
    else
        {state with 
           Graph = Graph.claimEdge state.Graph claim.punter edge
           Graph2 = Graphs.Graph.claimEdge state.Graph2 claim.punter eid}

let private applyClaims state claims = List.fold applyClaim state claims

let initialState (setup: ProtocolData.SetupIn ) =
    let verts =
        setup.map.sites
        |> Array.map (fun {id = id; coords = coords} ->
            { Graph.Id = id;
              Graph.IsSource = Array.contains id setup.map.mines;
              Graph.Coords = Option.map (fun (c: ProtocolData.Coords) -> (c.x, c.y)) coords })
    let edges = setup.map.rivers |> Array.map (fun site -> (site.source, site.target)) 

    let vIndex = Array.map (fun {Graph.Id=id} -> id) verts |> Index.create
    let eIndex = Index.create edges
    
    let nVertices = Array.length verts 
    let sources =
        {0..nVertices - 1}
        |> Seq.filter (fun vi -> verts.[vi].IsSource)
        |> Seq.toArray
    let edges2 = 
        Array.map (fun (u, v) -> (vIndex.i(u), vIndex.i(v))) edges

    let G = Graph.create verts (Array.toList edges)
    {
        Graph = G
        Graph2 = Graphs.Graph.create nVertices sources edges2
        EIndex = eIndex
        Me = setup.punter
        BFSDist = ShortestPath.Compute G
        Union = FastUnion.T G
        NumPlayers = setup.punters
    }

let applyMoveIn state (moveIn: ProtocolData.MoveIn) =
    moveIn.move.moves
    |> Array.toList
    |> List.choose (function
        | ProtocolData.Claim claim -> Some claim
        | ProtocolData.Pass _ -> None)
    |> applyClaims state

let score game (dist: Map<Graph.VertexId, int[]>) (reach: Map<Graph.VertexId, int[]>) =
    let isSource { Graph.IsSource = s } = s in
    let (sources, sinks) = Array.partition isSource game.Graph.Verts in
    let mutable total = 0;
    for u in sources do
        for v in sinks do
            let d = dist.[u.Id].[int v.Id] in
            if reach.[u.Id].[int v.Id] <> -1 then total <- total + d * d
    total  


type Renderer = {
    directory: string;
    mutable count: int;
}  with
    static member create directory =
        let dir = sprintf "%s/%d" directory (System.DateTime.Now.Ticks)
        System.IO.Directory.CreateDirectory dir |> ignore
        { directory = dir; count = 0 }
        
    member this.dump (game: State) =
        let dot = sprintf "%s/%d.dot" this.directory this.count in
        let svg = sprintf "%s/_%d.svg" this.directory this.count in
        System.IO.File.WriteAllText(dot, (Graph.toDot game.Me game.Graph))
        use p = System.Diagnostics.Process.Start("dot", sprintf "-Kfdp -n -Tsvg %s -o %s" dot svg)
        this.count <- this.count + 1
