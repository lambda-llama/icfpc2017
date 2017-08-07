module Game

open System
open System.IO

open Graphs
open Pervasives

(* Mapping from external to internal IDs. *)
[<Struct>]
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
    Futures: bool
    Splurges: bool
    Options: bool
    StrategyState: Map<string, string>
    TimeoutsCount: int
    TimeUsedLastMoveFraction: float
} with
    static member Deserialize blob: State =
        use s = new MemoryStream(Convert.FromBase64String(blob))
        use r = new BinaryReader(s)
        let graph = Graph.T.Read r
        let vIndex = r.ReadArray (fun _ -> r.ReadUInt32 ()) |> Index.create
        let me = r.ReadInt32 ()
        let numPlayers = r.ReadInt32 ()
        let futures = r.ReadBoolean ()
        let splurges = r.ReadBoolean ()
        let options = r.ReadBoolean ()
        let strategyState = r.ReadMap (fun _ -> (r.ReadString (), r.ReadString ()))
        let timeoutCount = r.ReadInt32 ()
        let timeUsedLastMoveFraction = r.ReadDouble ()
        {Graph=graph; VIndex=vIndex; Me=me; NumPlayers=numPlayers;
         Futures=futures; Splurges=splurges; Options=options;
         StrategyState=strategyState; TimeoutsCount=timeoutCount;
         TimeUsedLastMoveFraction=timeUsedLastMoveFraction}

    member state.Serialize (): string =
        use s = new MemoryStream()
        use w = new BinaryWriter(s)
        state.Graph.Write w
        w.WriteArray (state.VIndex.iToE, w.Write)
        w.Write state.Me
        w.Write state.NumPlayers
        w.Write state.Futures
        w.Write state.Splurges
        w.Write state.Options
        w.WriteMap (state.StrategyState, (fun k v -> w.Write k; w.Write v))
        w.Write state.TimeoutsCount
        w.Write state.TimeUsedLastMoveFraction

        w.Flush ()
        Convert.ToBase64String(s.GetBuffer ())

let initialState (setup: ProtocolData.SetupIn) (defaultStrategyState: Map<string, string>) =
    let vIndex = setup.map.sites |> Array.map (fun {id=id} -> id) |> Index.create
    let vertices =
        setup.map.sites
        |> Array.map (fun s ->
            let vid = vIndex.i(s.id)
            let isSource = Array.contains s.id setup.map.mines
            let coords =
                if !debug
                then s.coords |> Option.map (fun c -> (c.x, c.y))
                else None
            Vertex.create vid isSource coords)
    let edges =
        setup.map.rivers
        |> Array.mapi (fun i r ->
            let uv = (vIndex.i(r.source), vIndex.i(r.target))
            Edge.create i uv)
    {
        Graph = Graph.create vertices edges setup.settings.options
        VIndex = vIndex
        Me = setup.punter
        NumPlayers = setup.punters
        Futures = setup.settings.futures
        Splurges = setup.settings.splurges
        Options = setup.settings.options
        StrategyState = defaultStrategyState
        TimeoutsCount = 0
        TimeUsedLastMoveFraction = 0.0
    }

let private stepBudgetMs = 900.0
let applyStrategyStep s step =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let (edge, newStrategyState) = step s
    let isOption = Graph.isOptionFor s.Me s.Graph edge
    stopWatch.Stop()
    let usedFraction = float stopWatch.ElapsedMilliseconds / stepBudgetMs
    let (u, v) = Edge.ends edge
    let vIndex = s.VIndex
    let (eu, ev) = (vIndex.e(u), vIndex.e(v))
    let newState = { s with StrategyState = newStrategyState; TimeUsedLastMoveFraction = usedFraction }
    ((eu, ev), isOption, newState)

let applyClaim s (claim: ProtocolData.Claim) =
    let eid = (s.VIndex.i(claim.source), s.VIndex.i(claim.target))
              |> Graph.edgeId s.Graph
    {s with Graph=Graph.claimOptionEdge s.Graph claim.punter s.Me eid}

let applySplurge s (splurge: ProtocolData.Splurge) =
    Array.toList splurge.route |> pairwise |> List.fold (fun s (eu, ev) ->
        applyClaim s {punter=splurge.punter; source=eu; target=ev}) s

let applyMoves = Array.fold (fun s move ->
    match move with
    | ProtocolData.Claim claim
    | ProtocolData.Option claim -> applyClaim s claim
    | ProtocolData.Splurge splurge -> applySplurge s splurge
    | ProtocolData.Pass _ -> s)

let score2 game (dist: Map<int, int[]>) (reach: Map<int, int[]>) =
    let mutable total = 0
    for uid in Graph.sources game.Graph do
        for vid in Graph.sinks game.Graph do
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
