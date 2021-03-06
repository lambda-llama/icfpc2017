module Offline

open Graphs
open Pervasives

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
      let blob = (Game.initialState setup strategy.defaultState).Serialize ()
      Pipe.write p (ProtocolData.Ready {ready=setup.punter; state=Some blob; futures=[||]})
    | ProtocolData.RequestMove {move=move; state=Some blob} ->
      let state = Game.applyMoves (time "State.Load" (fun () -> Game.State.Deserialize blob)) move.moves
      let timeoutsCount =
        move.moves
        |> Array.filter (fun m -> match m with | ProtocolData.Pass pass -> pass.punter = state.Me | _ -> false)
        |> Array.length
      let state = { state with TimeoutsCount = timeoutsCount }
      let step = strategy.init state.Graph
      let ((eu, ev), isOption, newState) = Game.applyStrategyStep state step
      let nextMove =
        if isOption
        then ProtocolData.Option {punter=state.Me; source=eu; target=ev}
        else ProtocolData.Claim {punter=state.Me; source=eu; target=ev}
      Pipe.write p (ProtocolData.Move {move=nextMove; state=Some (time "State.Save" newState.Serialize)})
    | _ -> ()
    Pipe.close p
