module ProtocolData

// 0. Handshake

type HandshakeOut = {
    me : string
}

type HandshakeIn = {
    you : string
}

// 1. Setup

type Site = {
    id : int
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

type SetupIn = {
    punter : int
    punters : int
    map : Map
}

type SetupOut = {
    ready : int
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
}

type MoveOut = Move

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
    | Move of MoveIn
    | Stop of StopIn
    | Timeout of TimeoutIn

type MessageOut = MoveOut
