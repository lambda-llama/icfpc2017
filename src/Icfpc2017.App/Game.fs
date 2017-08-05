module Game

open Graphs

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
type Color = ProtocolData.Color

type State = {
    Graph: Graph.T
    VIndex: Index<VertexId>
    Me: Color
    NumPlayers: int
    Settings: ProtocolData.Settings
}

let applyClaim state (claim: ProtocolData.Claim) =
    let Edge = (state.VIndex.i(claim.source), state.VIndex.i(claim.target))
    let eid = Graph.edgeId state.Graph Edge
    {state with
       Graph = Graph.claimEdge state.Graph claim.punter eid}

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
        Graph = Graph.create coords sources edges
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
    let (sources, sinks) = Array.partition Vertex.isSource (Graph.vertices game.Graph)
    let mutable total = 0
    for u in sources do
        let uid = Vertex.id u
        for v in sinks do
            let vid = Vertex.id v
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
        System.IO.File.WriteAllText(dot, (Graph.toDot game.Me game.Graph))
        use p = System.Diagnostics.Process.Start("dot", sprintf "-Kfdp -n -Tsvg %s -o %s" dot svg)
        this.count <- this.count + 1
