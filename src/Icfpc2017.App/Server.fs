module Server

open System.IO
open System.Net
open System.Net.Sockets
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Pipe
open ProtocolData

type MList<'a> = System.Collections.Generic.List<'a>

let start (mapFilePath : string) (port : int) : unit =
    let map = deserializeMap (JsonConvert.DeserializeObject<JObject>(File.ReadAllText(mapFilePath)))
    let numPlayers = 2
    let numMoves = map.rivers.Length
    let players = new MList<T>(2)
    let server = new TcpListener(IPAddress.Loopback, port)
    server.Start()
    while players.Count < numPlayers do
        async {
            let! sock = accept server
            let! (Handshake h) = serverRead sock
            let! _ = serverWrite sock (HandshakeAck { you = h.me })
            let! _ =
                serverWrite sock 
                    (Setup {
                        punter = players.Count
                        punters = numPlayers
                        map = map
                    })
            let! (Ready r) = serverRead sock
            assert (r.ready = players.Count)
            players.Add(sock)
        } |> Async.RunSynchronously
    let lastMoves =
        [0..numPlayers - 1]
        |> Seq.map (fun p -> Pass { punter = p })
        |> Seq.toArray
    for i in [0..numMoves - 1] do
        async {
            let index = i % numPlayers
            let sock = players.[index]
            let! _ =
                serverWrite sock
                    (RequestMove {
                        move =
                            {
                                moves = lastMoves
                            }
                    })
            let! (Move move) = serverRead sock
            match move with
            | Claim claim -> () // TODO: update state
            | Pass pass -> () // TODO: update state
            lastMoves.[index] <- move
        } |> Async.RunSynchronously
    for i in [0..numPlayers - 1] do
        async {
            let! _ =
                serverWrite players.[i]
                    (Stop {
                        stop =
                            {
                                moves = [||]
                                scores =
                                    [0..numPlayers - 1]
                                    |> Seq.map (fun p -> { punter = p; score = 0 })
                                    |> Seq.toArray
                            }
                    })
            ()
        } |> Async.RunSynchronously
