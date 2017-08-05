module Game

type State = {
    Graph: Graph.T
    Me: Graph.Color
    BFSDist: Map<Graph.VertexId, int[]>
    Union: FastUnion.T
    NumPlayers: int
}

let private applyClaim state (claim: ProtocolData.Claim) = 
    if claim.punter = state.Me
        then
            state.Union.Unite claim.source, claim.target 
            {
                Graph = Graph.claimEdge state.Graph claim.punter (claim.source, claim.target); 
                Me = state.Me
                BFSDist = state.BFSDist
                Union = state.Union
                NumPlayers =state.NumPlayers
            }
        else
            {
                Graph = Graph.claimEdge state.Graph claim.punter (claim.source, claim.target); 
                Me = state.Me
                BFSDist = state.BFSDist
                Union = state.Union
                NumPlayers =state.NumPlayers
            }

let private applyClaims state claims = List.fold applyClaim state claims

let initialState (setup: ProtocolData.SetupIn ) =
    let verts =
        setup.map.sites
        |> Array.map (fun {id = id; coords = coords} ->
            { Graph.Id = id;
              Graph.IsSource = Array.contains id setup.map.mines;
              Graph.Coords = Option.map (fun (c: ProtocolData.Coords) -> (c.x, c.y)) coords; })
    let edges =
        setup.map.rivers
            |> Array.toList
            |> List.map (fun site -> (site.source, site.target))
    let G = Graph.create verts edges
    {
        Graph = G;
        Me = setup.punter;
        BFSDist = ShortestPath.Compute G;
        Union = FastUnion.T G;
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
