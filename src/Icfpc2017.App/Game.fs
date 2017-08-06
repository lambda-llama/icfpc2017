module Game

open Newtonsoft.Json

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
    StrategyState: Map<string, string>
    TimeoutsCount: int
    TimeUsedLastMovePct: float
} with
    static member Deserialize s: State =
        JsonConvert.DeserializeObject<State> (s, Graph.Converter (), Vertex.Converter (), Edge.Converter ())

    member s.Serialize () =
        JsonConvert.SerializeObject (s, Graph.Converter (), Vertex.Converter (), Edge.Converter ())

let initialState (setup: ProtocolData.SetupIn) =
    let vIndex = setup.map.sites |> Array.map (fun {id=id} -> id) |> Index.create
    let vertices =
        setup.map.sites
        |> Array.map (fun s ->
            let vid = vIndex.i(s.id)
            let isSource = Array.contains s.id setup.map.mines
            let coords = s.coords |> Option.map (fun c -> (c.x, c.y))
            Vertex.create vid isSource coords)
    let edges =
        setup.map.rivers
        |> Array.mapi (fun i r ->
            let uv = (vIndex.i(r.source), vIndex.i(r.target))
            Edge.create i uv)
    {
        Graph = Graph.create vertices edges
        VIndex = vIndex
        Me = setup.punter
        NumPlayers = setup.punters
        Settings = setup.settings
        StrategyState = Map.empty
        TimeoutsCount = 0
        TimeUsedLastMovePct = 0.0
    }


let applyClaim s (claim: ProtocolData.Claim) =
    let eid = (s.VIndex.i(claim.source), s.VIndex.i(claim.target))
              |> Graph.edgeId s.Graph
    {s with Graph = Graph.claimEdge s.Graph claim.punter eid}

let applyMoves = Array.fold (fun s move ->
    match move with
    | ProtocolData.Claim claim -> applyClaim s claim
    | ProtocolData.Pass _ -> s)

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
        //use p = System.Diagnostics.Process.Start("dot", sprintf "-Kfdp -n -Tsvg %s -o %s" dot svg)
        this.count <- this.count + 1
