module Simulation

let rec private simulateSteps nSteps (game: Game.State) (punters: (Graph.Color * Strategy.T) list) = 
    let (me, strat) = List.head punters
    if nSteps = 0 then game
    else 
        let (u, v) = strat { game with Me = me }
        let nextState = Game.applyClaim game { punter = me; source = u; target = v }
        simulateSteps (nSteps - 1) nextState (List.append (List.tail punters) [(me, strat)])


let simulate (map: ProtocolData.Map) (strats: Strategy.T list): int list =
    let (setup: ProtocolData.SetupIn) = 
        { punter = -1;
          punters = List.length strats;
          map = map } in
    let state = Game.initialState setup
    let endGame = simulateSteps 10 state (List.mapi (fun i s -> (i, s)) strats)
    let dists = ShortestPath.Compute (endGame.Graph)
    strats
    |> List.mapi (fun i _ ->
        let reach = ShortestPath.Compute { endGame.Graph with Edges = endGame.Graph.Edges |> List.filter (fun e -> e.Color = Some(i))}
        Game.score endGame dists reach
    )