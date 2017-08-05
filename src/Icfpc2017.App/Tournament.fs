module Tournament

open System
open Strategies
open Game
open Pipe

let selectPlayers (nPlayers: int) = 
    let random = System.Random()
    let availableStrategies = Strategies.all  |> Map.toList |> List.map (fun x -> (fst x))
    let n = availableStrategies.Length 
    let selectedStrategiesNames =  Seq.initInfinite (fun _ -> random.Next(n)) 
                                |> Seq.take(int nPlayers) 
                                |> Seq.toList
                                |> List.map (fun x -> availableStrategies.Item x)
    printf "Playing with the following strategies %A" selectedStrategiesNames ;
    selectedStrategiesNames

let handshake (p: Pipe.T) (username: string): Async<ProtocolData.SetupIn> = async {
    let! _ = Pipe.write p (ProtocolData.Handshake {me=username})
    let! (ProtocolData.HandshakeAck h) = Pipe.read p
    if h.you <> username
    then return (failwithf "Unexpected response: %A\n" h)
    else
        let! (ProtocolData.Setup setup) = Pipe.read p
        let! _ = Pipe.write p (ProtocolData.Ready {ready=setup.punter})
        return setup
}
let play (p: Pipe.T) punter (strategy: Strategy.T) =
    let rend = Game.Renderer.create "game"
    let rec go currState =
        async {
            let! message = Pipe.read p in
            match message with
            | ProtocolData.RequestMove moves ->
              let nextState = Game.applyMoveIn currState moves
              rend.dump nextState
              let (source, target) = strategy nextState
              let nextMove = ProtocolData.Claim {
                  punter=punter; 
                  source=int source; 
                  target=int target
              }
              let! () = Pipe.write p (ProtocolData.Move nextMove)
              return! go nextState
            | ProtocolData.Stop stop -> return ()
            | _ -> failwithf "Unexpected response: %A\n" message
        }
    in go


let online host port name strategy = async {
    let! p = Pipe.connect host port
    let! setup = handshake p name
    let initialState = Game.initialState setup
    let! _ = play p setup.punter strategy initialState
    printf "We: %d" (setup.punter)
    return ()
}


let playTournament (strategyNames: string list) (port: int) = 
    List.map (fun name -> Strategies.all.[name]) strategyNames
    |> List.zip strategyNames
    |> List.map (fun strategy_name -> online "punter.inf.ed.ac.uk" port (fst strategy_name) (snd strategy_name))
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore


let tournament (nPlayers: int) (port: int) = 
    let testedStrategies = selectPlayers nPlayers
    playTournament testedStrategies port