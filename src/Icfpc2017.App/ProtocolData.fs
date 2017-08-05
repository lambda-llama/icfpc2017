module ProtocolData

open System.Linq
open Newtonsoft.Json
open Newtonsoft.Json.Linq

// 0. Handshake

type HandshakeOut = {
    me : string
}

type HandshakeIn = {
    you : string
}

// 1. Setup

type Coords = {
    x : float
    y : float
}

type Site = {
    id : int
    coords : Coords option
}

type River = {
    source : int
    target : int
}

type Map = {
    sites : Site array
    rivers : River array
    mines : int array
}

type Settings = {
    futures : bool
}

type SetupIn = {
    punter : int
    punters : int
    map : Map
    settings : Settings
}

type SetupOut = {
    ready : int
    state : string option
}

// 2. Gameplay

type Claim = {
    punter : int
    source : int
    target : int
}

type Pass = {
    punter : int
}

type Move =
    | Claim of claim : Claim
    | Pass of pass : Pass

type Moves = {
    moves : Move array
}

type MoveIn = {
    move : Moves
    state : string option
}

type MoveOut = {
    move: Move
    state: string option
}

// 3. Scoring

type Score = {
    punter : int
    score : int
}

type Stop = {
    moves : Move array
    scores : Score array
}

type StopIn = {
    stop : Stop
}

// Misc

type TimeoutIn = {
    timeout : int
}

type MessageIn =
    | HandshakeAck of HandshakeIn
    | Setup of SetupIn
    | RequestMove of MoveIn
    | Stop of StopIn
    | Timeout of TimeoutIn

type MessageOut =
    | Handshake of HandshakeOut
    | Ready of SetupOut
    | Move of MoveOut

// Serialization

let serializeArray (arr : 'a array) (doSerialize : 'a -> 'b) : JArray =
    arr
    |> Seq.map doSerialize
    |> Seq.toArray
    |> JArray

let serializeHandshakeIn (h : HandshakeIn) : JObject =
    JObject(
        JProperty("you", h.you))

let serializeHandshakeOut (h : HandshakeOut) : JObject =
    JObject(
        JProperty("me", h.me))

let serializeSite (s : Site) : JObject =
    match s.coords with
    | Some coords ->
        JObject(
            JProperty("id", s.id),
            JProperty("x", coords.x),
            JProperty("y", coords.y))
    | None ->
        JObject(
            JProperty("id", s.id))

let serializeRiver (r : River) : JObject =
    JObject(
        JProperty("source", r.source),
        JProperty("target", r.target))

let serializeMap (m : Map) : JObject =
    JObject(
        JProperty("sites", serializeArray m.sites serializeSite),
        JProperty("rivers", serializeArray m.rivers serializeRiver),
        JProperty("mines", serializeArray m.mines (fun m -> m)))

let serializeSettings (s : Settings) : JObject =
    JObject(
        JProperty("futures", s.futures))

let serializeSetupIn (s : SetupIn) : JObject =
    JObject(
        JProperty("punter", s.punter),
        JProperty("punters", s.punters),
        JProperty("map", serializeMap s.map),
        JProperty("settings", serializeSettings s.settings))

let serializeSetupOut (s : SetupOut) : JObject =
    match s.state with 
    | Some state -> 
      JObject(
          JProperty("ready" , s.ready),
          JProperty("state", state))
    | None -> 
      JObject(
          JProperty("ready" , s.ready))

let serializeClaim (c : Claim) : JObject =
    JObject(
        JProperty("claim",        
            JObject(
                JProperty("punter", c.punter),
                JProperty("source", c.source),
                JProperty("target", c.target))))

let serializePass (p : Pass) : JObject =
    JObject(
        JProperty("pass",
            JObject(
                JProperty("punter", p.punter))))

let serializeMove (m : Move) : JObject =
    match m with
    | Claim claim -> serializeClaim claim
    | Pass pass -> serializePass pass

let serializeMoves (m : Moves) : JObject =
    JObject(
        JProperty("moves", serializeArray m.moves serializeMove))

let serializeMoveIn (m : MoveIn) : JObject =
    JObject(
        JProperty("move", serializeMoves m.move))

let serializeMoveOut (m : MoveOut) : JObject =
    let o = 
        match m.move with
        | Claim claim -> serializeClaim claim
        | Pass pass -> serializePass pass
    in match m.state with 
       | Some state -> o.Add(JProperty("state", state)); o
       | None -> o

let serializeScore (s : Score) : JObject =
    JObject(
        JProperty("punter", s.punter),
        JProperty("score", s.score))

let serializeStop (s : Stop) : JObject =
    JObject(
        JProperty("moves", serializeArray s.moves serializeMove),
        JProperty("scores", serializeArray s.scores serializeScore))

let serializeStopIn (s : StopIn) : JObject =
    JObject(
        JProperty("stop", serializeStop s.stop))

let serializeTimeoutIn (t : TimeoutIn) : JObject =
    JObject(
        JProperty("timeout", t.timeout))

let serializeMessageIn (m : MessageIn) : JObject =
    match m with
    | HandshakeAck handshakeIn -> serializeHandshakeIn handshakeIn
    | Setup setupIn -> serializeSetupIn setupIn
    | RequestMove moveIn -> serializeMoveIn moveIn
    | Stop stopIn -> serializeStopIn stopIn
    | Timeout timeoutIn -> serializeTimeoutIn timeoutIn

let serializeMessageOut (m : MessageOut) : JObject =
    match m with
    | Handshake handshakeOut -> serializeHandshakeOut handshakeOut
    | Ready setupOut -> serializeSetupOut setupOut
    | Move moveOut -> serializeMoveOut moveOut

let serialize (m : MessageOut) : string =
    JsonConvert.SerializeObject(serializeMessageOut(m))

let serverSerialize (m : MessageIn) : string =
    JsonConvert.SerializeObject(serializeMessageIn(m))

// Deserialization

let convertArray (arr : JToken) (doDeserialize : 'a -> 'b) : 'b array =
    (arr :?> JArray)
        |> Seq.cast<'a>
        |> Seq.map doDeserialize
        |> Seq.toArray

let deserializeHandshakeIn (o : JObject) : HandshakeIn =
    {
        you = o.["you"].ToObject<string>()
    }

let deserializeHandshakeOut (o : JObject) : HandshakeOut =
    {
        me = o.["me"].ToObject<string>()
    }

let deserializeCoords (o : JObject) : Coords option =
    match o.["x"] with
    | null -> None
    | _ ->
        Some (
            {
                x = o.["x"].ToObject<float>()
                y = o.["y"].ToObject<float>()                
            })

let deserializeSite (o : JObject) : Site =
    {
        id = o.["id"].ToObject<int>()
        coords = o |> deserializeCoords
    }

let deserializeRiver (o : JObject) : River =
    {
        source = o.["source"].ToObject<int>()
        target = o.["target"].ToObject<int>()
    }

let deserializeMap (o : JObject) : Map =
    {
        sites = convertArray o.["sites"] deserializeSite
        rivers = convertArray o.["rivers"] deserializeRiver
        mines = convertArray o.["mines"] (fun (v : JToken) -> v.ToObject<int>())
    }

let deserializeSettings (o : JObject) =
    if o = null then { futures = false }
    else
    {
        futures = o.["futures"].ToObject<bool>()
    }

let deserializeState (o : JObject) = 
    if o.["state"] = null then None else Some (o.["state"].ToObject<string> ())

let deserializeSetupIn (o : JObject) : SetupIn =
    {
        punter = o.["punter"].ToObject<int>()
        punters = o.["punters"].ToObject<int>()
        map = (o.["map"] :?> JObject) |> deserializeMap
        settings = (o.["settings"] :?> JObject) |> deserializeSettings
    }

let deserializeSetupOut (o : JObject) : SetupOut =
    {
        ready = o.["ready"].ToObject<int>()
        state = deserializeState o
    }

let deserializeMove (o : JObject) : Move =
    let prop = o.Properties().First()
    let v = prop.Value :?> JObject
    match (prop.Name) with
    | "claim" ->
        Claim {
            punter = v.["punter"].ToObject<int>()
            source = v.["source"].ToObject<int>()
            target = v.["target"].ToObject<int>()
        }
    | "pass" ->
        Pass {
            punter = v.["punter"].ToObject<int>()
        }
    | x -> raise (exn x)

let deserializeMoveOut (o : JObject) : MoveOut =
    let prop = o.Properties().First()
    let v = prop.Value :?> JObject
    let move = 
        match (prop.Name) with
        | "claim" ->
            Claim {
                punter = v.["punter"].ToObject<int>()
                source = v.["source"].ToObject<int>()
                target = v.["target"].ToObject<int>()
            }
        | "pass" ->
            Pass {
                punter = v.["punter"].ToObject<int>()
            }
        | x -> raise (exn x)
    in {move=move; state=deserializeState o}

let deserializeMoves (o : JObject) : Moves =
    {
        moves = convertArray o.["moves"] deserializeMove
    }

let deserializeMoveIn (o : JObject) : MoveIn =
   {
        move = (o.["move"] :?> JObject) |> deserializeMoves
        state = deserializeState o
    }

let deserializeScore (o : JObject) : Score =
    {
        punter = o.["punter"].ToObject<int>()
        score = o.["score"].ToObject<int>()
    }

let deserializeStop (o : JObject) : Stop =
    {
        moves = convertArray o.["moves"] deserializeMove
        scores = convertArray o.["scores"] deserializeScore
    }

let deserializeStopIn (o : JObject) : StopIn =
    {
        stop = (o.["stop"] :?> JObject) |> deserializeStop
    }

let deserializeTimeoutIn (o : JObject) : TimeoutIn =
    {
        timeout = o.["timeout"].ToObject<int>()
    }

let deserializeMessageIn (o : JObject) : MessageIn =
    let prop = o.Properties().First()
    match (prop.Name) with
    | "you" -> HandshakeAck (deserializeHandshakeIn o)
    | "punter" -> Setup (deserializeSetupIn o)
    | "move" -> RequestMove (deserializeMoveIn o)
    | "stop" -> Stop (deserializeStopIn o)
    | "timeout" -> Timeout (deserializeTimeoutIn o)
    | x -> raise (exn x)

let deserializeMessageOut (o : JObject) : MessageOut =
    let prop = o.Properties().First()
    match (prop.Name) with
    | "me" -> Handshake (deserializeHandshakeOut o)
    | "ready" -> Ready (deserializeSetupOut o)
    | "claim" | "pass" -> Move (deserializeMoveOut o)
    | x -> raise (exn x)

let deserialize (message : string) : MessageIn =
    deserializeMessageIn (JsonConvert.DeserializeObject<JObject>(message))

let serverDeserialize (message : string) : MessageOut =
    deserializeMessageOut (JsonConvert.DeserializeObject<JObject>(message))
