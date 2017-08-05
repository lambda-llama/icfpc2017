module Simulation

let rec private simulateSteps nStep totalSteps (game: Game.State) (punters: (Game.Color * string * (Game.State -> (Graphs.Edge.T))) list) = 
    let (me, name, step) = List.head punters
    if nStep = totalSteps then game
    else 
        printf "Step %d: %s\n" nStep name
        let (u, v) = step { game with Me = me } |> Graphs.Graph.originalEnds game.Graph2
        let nextState = Game.applyClaim game { punter = me; source = u; target = v }
        simulateSteps (nStep + 1) totalSteps nextState (List.append (List.tail punters) [(me, name, step)])


let simulate (map: ProtocolData.Map) (strats: Strategy.T list): int list =
    let (setup: ProtocolData.SetupIn) = 
        { punter = -1;
          punters = List.length strats;
          map = map;
          settings = { futures = false } } in
    let state = Game.initialState setup
    let nSteps = Array.length map.rivers
    let initStrat i (t: Strategy.T) = (i, t.name, t.init state.Graph2)
    let endGame = simulateSteps 0 nSteps state (List.mapi initStrat strats)
    let dists = Graphs.Traversal.shortestPaths (endGame.Graph2)
    strats
    |> List.mapi (fun i _ ->
        let reach = Graphs.Traversal.shortestPaths (Graphs.Graph.subgraph endGame.Graph2 i)
        Game.score2 endGame dists reach
    )