module Simulation

let rec private simulateSteps nStep totalSteps (game: Game.State) (punters: (Graph.Color * string * (Game.State -> (Graph.VertexId * Graph.VertexId))) list) = 
    let (me, name, step) = List.head punters
    if nStep = totalSteps then game
    else 
        printf "Step %s %d\n" name nStep
        let (u, v) = step { game with Me = me }
        let nextState = Game.applyClaim game { punter = me; source = u; target = v }
        simulateSteps (nStep + 1) totalSteps nextState (List.append (List.tail punters) [(me, name, step)])


let simulate (map: ProtocolData.Map) (strats: Strategy.T list): int list =
    let (setup: ProtocolData.SetupIn) = 
        { punter = -1;
          punters = List.length strats;
          map = map } in
    let state = Game.initialState setup
    let nSteps = Array.length map.rivers
    let initStrat i (t: Strategy.T) = (i, t.name, t.init state.Graph)
    let endGame = simulateSteps 0 nSteps state (List.mapi initStrat strats)
    let dists = ShortestPath.Compute (endGame.Graph)
    strats
    |> List.mapi (fun i _ ->
        let reach = ShortestPath.Compute { endGame.Graph with Edges = endGame.Graph.Edges |> List.filter (fun e -> e.Color = Some(i))}
        Game.score endGame dists reach
    )