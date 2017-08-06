module Simulation

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open Graphs

let rec private simulateSteps nStep totalSteps (game: Game.State) (punters: (Game.Color * string * (Game.State -> (Edge.T * Map<string, string>))) list) =
    let (me, name, step) = List.head punters
    if nStep = totalSteps then game
    else
        printf "Step %d: %s\n" nStep name
        let ((eu, ev), nextState) = Game.applyStrategyStep {game with Me=me} step
        let nextState =
            Game.applyClaim nextState {punter=me; source=eu; target=ev}
        simulateSteps (nStep + 1) totalSteps nextState (List.append (List.tail punters) [(me, name, step)])


let simulate (map: ProtocolData.Map) (strats: Strategy.T list): int list =
    let (setup: ProtocolData.SetupIn) =
        { punter = -1;
          punters = List.length strats;
          map = map;
          settings = { futures = false; splurges = false } }
    let state = Game.initialState setup Map.empty // TODO: pass the default state
    let nSteps = Array.length map.rivers
    let initStrat i (t: Strategy.T) = (i, t.name, t.init state.Graph)
    let endGame = simulateSteps 0 nSteps state (List.mapi initStrat strats)
    let dists = Traversal.shortestPaths (endGame.Graph)
    strats
    |> List.mapi (fun i _ ->
        let reach = Traversal.shortestPaths (Graph.subgraph endGame.Graph i)
        Game.score2 endGame dists reach
    )

let run mapPath =
    let map = System.IO.File.ReadAllText mapPath
    let competitors = [
        GreadyStrategy.greadyStrategy
        GreadyStrategy.greadyStrategyWithHeur
        // Strategy.randomEdge
    ]
    let (scores: int list) = simulate (JsonConvert.DeserializeObject<JObject>(map) |> ProtocolData.deserializeMap) competitors
    List.zip competitors scores
    |> List.iter (fun (c, s) ->  printf "%s %d\n" c.name s)
