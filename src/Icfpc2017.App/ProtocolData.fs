module ProtocolData

open System.Linq

open Newtonsoft.Json
open Newtonsoft.Json.Linq

open Pervasives

// 0. Handshake

type HandshakeOut = {
    me : string
}

type HandshakeIn = {
    you : string
}

// 1. Setup

type VertexId = uint32
type Color = int

type Coords = {
    x : float
    y : float
}

type Site = {
    id : VertexId
    coords : Coords option
}

type River = {
    source : VertexId
    target : VertexId
}

type Map = {
    sites : Site array
    rivers : River array
    mines : VertexId array
}

type Settings = {
    futures : bool
    splurges : bool
    options : bool
}

type SetupIn = {
    punter : Color
    punters : int
    map : Map
    settings : Settings
}

type SetupOut = {
    ready : int
    state : string option
    futures : River array
}

// 2. Gameplay

type Claim = {
    punter : Color
    source : VertexId
    target : VertexId
}

type Pass = {
    punter : Color
}

type Splurge = {
    punter: Color
    route: VertexId array
}

type Move =
    | Claim of claim : Claim
    | Pass of pass : Pass
    | Splurge of splurge : Splurge
    | Option of claim : Claim

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
    punter : Color
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
        JProperty("mines", serializeArray m.mines id))

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
          JProperty("state", state),
          JProperty("futures", serializeArray s.futures serializeRiver))
    | None ->
      JObject(
          JProperty("ready" , s.ready),
          JProperty("futures", serializeArray s.futures serializeRiver))

let serializeClaim tag (c : Claim) : JObject =
    JObject(
        JProperty(tag,
            JObject(
                JProperty("punter", c.punter),
                JProperty("source", c.source),
                JProperty("target", c.target))))

let serializePass (p : Pass) : JObject =
    JObject(
        JProperty("pass",
            JObject(
                JProperty("punter", p.punter))))


let serializeSplurge (s : Splurge) : JObject =
    JObject(
        JProperty("splurge",
            JObject(
                JProperty("punter", s.punter),
                JProperty("routes", serializeArray s.route id))))

let serializeMove (m : Move) : JObject =
    match m with
    | Claim claim -> serializeClaim "claim" claim
    | Option claim -> serializeClaim "option" claim
    | Pass pass -> serializePass pass
    | Splurge splurge -> serializeSplurge splurge

let serializeMoves (m : Moves) : JObject =
    JObject(
        JProperty("moves", serializeArray m.moves serializeMove))

let serializeMoveIn (m : MoveIn) : JObject =
    JObject(
        JProperty("move", serializeMoves m.move))

let serializeMoveOut (m : MoveOut) : JObject =
    let o = serializeMove m.move
    match m.state with
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
    time "Message.Serialize"
        (fun () -> JsonConvert.SerializeObject(serializeMessageOut(m)))

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
        id = o.["id"].ToObject<VertexId>()
        coords = o |> deserializeCoords
    }

let deserializeRiver (o : JObject) : River =
    {
        source = o.["source"].ToObject<VertexId>()
        target = o.["target"].ToObject<VertexId>()
    }

let deserializeMap (o : JObject) : Map =
    {
        sites = convertArray o.["sites"] deserializeSite
        rivers = convertArray o.["rivers"] deserializeRiver
        mines = convertArray o.["mines"] (fun (v : JToken) -> v.ToObject<VertexId>())
    }

let deserializeSettings (o : JObject) =
    if isNull o
    then {futures=false; splurges=false; options=false}
    else
        let futures =
            not (isNull o.["futures"]) && o.["futures"].ToObject<bool>()
        let splurges =
            not (isNull o.["splurges"]) && o.["splurges"].ToObject<bool>()
        let options =
            not (isNull o.["options"]) && o.["options"].ToObject<bool>()
        {futures=futures; splurges=splurges; options=options}

let deserializeState (o : JObject) =
    if isNull o.["state"] then None else Some (o.["state"].ToObject<string> ())

let deserializeSetupIn (o : JObject) : SetupIn =
    {
        punter = o.["punter"].ToObject<Color>()
        punters = o.["punters"].ToObject<int>()
        map = (o.["map"] :?> JObject) |> deserializeMap
        settings = (o.["settings"] :?> JObject) |> deserializeSettings
    }

let deserializeMove (o : JObject) : Move =
    let prop = o.Properties().First()
    let v = prop.Value :?> JObject
    match (prop.Name) with
    | "claim" ->
        Claim {
            punter = v.["punter"].ToObject<Color>()
            source = v.["source"].ToObject<VertexId>()
            target = v.["target"].ToObject<VertexId>()
        }
    | "pass" ->
        Pass {
            punter = v.["punter"].ToObject<Color>()
        }
    | "splurge" ->
        Splurge {
            punter = v.["punter"].ToObject<Color>()
            route = convertArray v.["route"] (fun (i: JValue) -> i.ToObject<VertexId>())
        }
    | "option" ->
        Option {
            punter = v.["punter"].ToObject<Color>()
            source = v.["source"].ToObject<VertexId>()
            target = v.["target"].ToObject<VertexId>()
        }
    | x -> raise (exn x)

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
        punter = o.["punter"].ToObject<Color>()
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

let deserialize (message : string) : MessageIn =
    time "Message.Deserialize"
        (fun () -> deserializeMessageIn (JsonConvert.DeserializeObject<JObject>(message)))
