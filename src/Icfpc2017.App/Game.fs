module Game

type State = {
    Graph: Graph.T
    Me: Graph.Color
}

let private applyClaim state (claim: ProtocolData.Claim) = {
    state with Graph = Graph.claimEdge state.Graph (uint32 claim.punter) (uint32 claim.source, uint32 claim.target);
}

let private applyClaims state claims = List.fold applyClaim state claims

let initialState (setup: ProtocolData.SetupIn ) =
    let verts = 
        setup.map.sites
        |> Array.map (fun {id = id; coords = coords} -> 
            { Graph.Id = uint32 id; 
              Graph.IsSource = Array.contains id setup.map.mines; 
              Graph.Coords = Option.map (fun (c: ProtocolData.Coords) -> (c.x, c.y)) coords; })
    let edges =
        setup.map.rivers
            |> Array.toList
            |> List.map (fun site -> (uint32 site.source, uint32 site.target))
    in {
        Graph = Graph.create verts edges;
        Me = uint32 setup.punter;
    }
let applyMoveIn state (moveIn: ProtocolData.MoveIn) =
    moveIn.move.moves
    |> Array.toList
    |> List.choose (function
        | ProtocolData.Claim claim -> Some claim
        | ProtocolData.Pass _ -> None)
    |> applyClaims state


type Renderer = {
    directory: string;
    mutable count: int;
}  with
    static member create directory =
        System.IO.Directory.CreateDirectory directory |> ignore
        { directory = directory; count = 0 }
    member this.dump (game: State) =
        let dot = sprintf "%s/%d.dot" this.directory this.count in
        let png = sprintf "%s/_%d.png" this.directory this.count in
        System.IO.File.WriteAllText(dot, (Graph.toDot game.Me game.Graph))
        use p = System.Diagnostics.Process.Start("dot", sprintf "-Kfdp -n -Tpng %s -o %s" dot png)
        this.count <- this.count + 1
