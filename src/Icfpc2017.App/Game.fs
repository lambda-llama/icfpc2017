module Game

module Graph2 = Graphs.Graph
module Vertex2 = Graphs.Vertex
module Edge2 = Graphs.Edge

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

type VertexId = ProtocolData.VertexId
type Color = int

type State = {
    Graph2: Graph2.T
    VIndex: Index<VertexId>
    Me: Color
    NumPlayers: int
    Settings: ProtocolData.Settings
}

let applyClaim state (claim: ProtocolData.Claim) =
    let edge2 = (state.VIndex.i(claim.source), state.VIndex.i(claim.target))
    let eid = Graph2.edgeId state.Graph2 edge2
    {state with
       Graph2 = Graph2.claimEdge state.Graph2 claim.punter eid}

let private applyClaims state claims = List.fold applyClaim state claims

let initialState (setup: ProtocolData.SetupIn ) =   
    let coords = 
        setup.map.sites 
        |> Array.map (fun s -> s.coords |> Option.map (fun c -> (c.x, c.y)))

    let vIndex = setup.map.sites |> Array.map (fun {id=id} -> id) |> Index.create
    let sources = setup.map.mines |> Array.map (fun vid -> vIndex.i(vid))    
    let edges = 
        setup.map.rivers
        |> Array.map (fun site -> (vIndex.i(site.source), vIndex.i(site.target)))   
    {
        Graph2 = Graph2.create coords sources edges
        VIndex = vIndex
        Me = setup.punter
        NumPlayers = setup.punters
        Settings = setup.settings
    }

let applyMoveIn state (moveIn: ProtocolData.MoveIn) =
    moveIn.move.moves
    |> Array.toList
    |> List.choose (function
        | ProtocolData.Claim claim -> Some claim
        | ProtocolData.Pass _ -> None)
    |> applyClaims state

let score2 game (dist: Map<int, int[]>) (reach: Map<int, int[]>) =
    let (sources, sinks) = Array.partition Vertex2.isSource (Graph2.vertices game.Graph2)
    let mutable total = 0
    for u in sources do
        let uid = Vertex2.id u
        for v in sinks do
            let vid = Vertex2.id v
            let d = dist.[uid].[vid]
            if reach.[uid].[vid] <> -1 then total <- total + d * d
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
        System.IO.File.WriteAllText(dot, (Graphs.Graph.toDot game.Me game.Graph2))
        use p = System.Diagnostics.Process.Start("dot", sprintf "-Kfdp -n -Tsvg %s -o %s" dot svg)
        this.count <- this.count + 1
