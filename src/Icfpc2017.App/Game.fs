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
        {eToI=Map.empty; iToE=iToE}

    member x.i (key: 'a) = Array.findIndex ((=) key) x.iToE
    member x.e (key: int) = x.iToE.[key]

    static member Read (readA: BinaryReader -> 'a) r: Index<'a> =
        failwith ":("

    member index.Write (writeA: BinaryWriter -> 'a -> unit) =
        failwith ":("
    

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
    TimeUsedLastMoveFraction: float
} with
    static member Deserialize blob: State =
        use s = new MemoryStream(Convert.FromBase64String(blob))
        use r = new BinaryReader(s)
        let graph = Graph.T.Read r
        let vIndex = Index.Read (fun r -> r.ReadUInt32()) r
        let me = r.ReadInt32()
        let numPlayers = r.ReadInt32()
        let settings = { ProtocolData.futures = r.ReadBoolean() }

        let count = r.ReadInt32()
        let xs = Array.create count ("", "")
        for i = 0 to (count - 1) do
            xs.[i] <- (r.ReadString(), r.ReadString())
        let strategyState = Map.ofArray xs
        let timeoutCount = r.ReadInt32()
        let timeUsedLastMoveFraction = r.ReadDouble()
        { Graph = graph; VIndex = vIndex; Me = me; NumPlayers = numPlayers; Settings = settings; 
          StrategyState = strategyState; TimeoutsCount = timeoutCount; TimeUsedLastMoveFraction = timeUsedLastMoveFraction}

    member state.Serialize (): string =
        use s = new MemoryStream()
        use w = new BinaryWriter(s)
        state.Graph.Write w
        state.VIndex.Write (fun w i -> w.Write(i))
        w.Write state.Me
        w.Write state.NumPlayers
        w.Write state.Settings.futures

        w.Write(state.StrategyState.Count)
        for kv in state.StrategyState do
            w.Write kv.Key
            w.Write kv.Value

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
        Graph = Graph.create vertices edges
        VIndex = vIndex
        Me = setup.punter
        NumPlayers = setup.punters
        Settings = setup.settings
        StrategyState = defaultStrategyState
        TimeoutsCount = 0
        TimeUsedLastMoveFraction = 0.0
    }

let private stepBudgetMs = 900.0

let applyStrategyStep s step =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    let (edge, newStrategyState) = step s
    stopWatch.Stop()
    let usedFraction = float stopWatch.ElapsedMilliseconds / stepBudgetMs
    let (u, v) = Edge.ends edge
    let vIndex = s.VIndex
    let (eu, ev) = (vIndex.e(u), vIndex.e(v))
    let newState = { s with StrategyState = newStrategyState; TimeUsedLastMoveFraction = usedFraction }
    ((eu, ev), newState)


let applyClaim s (claim: ProtocolData.Claim) =
    let eid = (s.VIndex.i(claim.source), s.VIndex.i(claim.target))
              |> Graph.edgeId s.Graph
    {s with Graph = Graph.claimEdge s.Graph claim.punter eid}

let applyMoves = Array.fold (fun s move ->
    match move with
    | ProtocolData.Claim claim -> applyClaim s claim
    | _ -> s)
    // | ProtocolData.Pass {punter=punter} ->
    //   (* REMOVE BEFORE FINAL SUBMISSION *)
    //   if punter = s.Me then failwith "PASS" else s)

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
