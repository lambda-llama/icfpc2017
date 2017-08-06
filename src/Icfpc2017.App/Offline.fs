module Offline

open Graphs

let username = "lambda-llama"

let handshake (p: Pipe.T): unit =
    let () = Pipe.write p (ProtocolData.Handshake {me=username})
    let (ProtocolData.HandshakeAck h) = Pipe.read p
    if h.you <> username
    then failwithf "Unexpected response: %A\n" h

let run (strategy: Strategy.T) =
    let p = Pipe.std ()
    handshake p
    match Pipe.read p with
    | ProtocolData.Setup setup ->
      let chunk = (Game.initialState setup).Serialize ()
      Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=Some chunk; futures=[||]})
    | ProtocolData.RequestMove {move=move; state=Some chunk} ->
      let state = Game.applyMoves (Game.State.Deserialize chunk) move.moves
      let step = strategy.init state.Graph
      let (edge, newStrategyState) = step state
      let (u, v) = Edge.ends edge
      let vIndex = state.VIndex
      let (eu, ev) = (vIndex.e(u), vIndex.e(v))
      let nextMove = ProtocolData.Claim {punter=state.Me; source=eu; target=ev}
      let newState = { state with StrategyState = newStrategyState }
      Pipe.write p (ProtocolData.Move {move=nextMove; state=Some (newState.Serialize ())})
    | _ -> ()
    Pipe.close p
