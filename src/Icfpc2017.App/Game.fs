module Game

type State = {
    Graph: Graph.T
    Mines: Graph.VertexId list
}

let private applyClaim state (claim: ProtocolData.Claim) = {
    Graph = Graph.claimEdge state.Graph (uint32 claim.punter) (uint32 claim.source, uint32 claim.target);
    Mines = state.Mines;
} 

let private applyClaims state claims = List.fold applyClaim state claims

let initialState (map: ProtocolData.Map) = 
    let edges = 
        map.rivers 
            |> Array.toList 
            |> List.map (fun site -> (uint32 site.source, uint32 site.target)) 
    let mines = map.mines |> Array.toList |> List.map uint32
    in { Graph = Graph.create (uint32 map.sites.Length) edges; Mines = mines }
let applyMoveIn state (moveIn: ProtocolData.MoveIn) =
    moveIn.move.moves
    |> Array.toList
    |> List.choose (function
        | ProtocolData.Claim claim -> Some claim
        | ProtocolData.Pass _ -> None)
    |> applyClaims state  