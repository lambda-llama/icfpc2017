module Simulation

let rec private simulateSteps nStep totalSteps (game: Game.State) (punters: (Graph.Color * Strategy.T) list) = 
    let (me, strat) = List.head punters
    if nStep = totalSteps then game
    else 
        printf "Step %d\n" nStep
        let (u, v) = strat { game with Me = me }
        let nextState = Game.applyClaim game { punter = me; source = u; target = v }
        simulateSteps (nStep + 1) totalSteps nextState (List.append (List.tail punters) [(me, strat)])


let simulate (map: ProtocolData.Map) (strats: Strategy.T list): int list =
    let (setup: ProtocolData.SetupIn) = 
        { punter = -1;
          punters = List.length strats;
          map = map } in
    let state = Game.initialState setup
    let nSteps = Array.length map.rivers
    let endGame = simulateSteps 0 nSteps state (List.mapi (fun i s -> (i, s)) strats)
    let dists = ShortestPath.Compute (endGame.Graph)
    strats
    |> List.mapi (fun i _ ->
        let reach = ShortestPath.Compute { endGame.Graph with Edges = endGame.Graph.Edges |> List.filter (fun e -> e.Color = Some(i))}
        Game.score endGame dists reach
    )