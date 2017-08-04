module ProtocolData

// 0. Handshake

type HandshakeOut =
    val me : string

type HandshakeIn =
    val out : string

// 1. Setup

type Site =
    val id : int

type River =
    val source : int
    val target : int

type Map =
    val sites : Site array
    val rivers : River array
    val mines : int array

type SetupIn =
    val punter : string
    val punters : int
    val map : Map

type SetupOut =
    val ready : string

// 2. Gameplay

type Claim =
    val punter : string
    val source : int
    val target : int

type Pass =
    val punter : string

type Move =
    val claim : Claim
    val pass : Pass

type MoveIn =
    val moves : Move array

type MoveOut =
    val move : Move

// 3. Scoring

type Score =
    val punter : string
    val score : int

type Stop =
    val moves : Move array
    val scores : Score array

type StopIn =
    val stop : Stop

// Misc

type TimeoutIn =
    val timeout : int

type MessageIn =
    | Move of MoveIn
    | Stop of StopIn
    | Timeout of TimeoutIn

type MessageOut = MoveOut
