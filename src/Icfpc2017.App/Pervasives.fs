module Pervasives

open System.IO
open System.Diagnostics

(* SET TO false BEFORE FINAL SUBMISSION *)
let debug = ref false

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let impossible<'a> : 'a = failwith "impossible"

let time message f =
    let timer = Stopwatch ()
    timer.Start()
    let result = f ()
    eprintf "%s: %d ms\n" message timer.ElapsedMilliseconds
    result

let pairwise xs =
    let rec go acc = function
    | [] | [_] -> List.rev acc
    | x::(y::_ as rest) -> go ((x, y)::acc) rest
    in go [] xs

type BinaryReader with
    member inline r.ReadArray(f: int -> 'a): 'a array =
        let length = r.ReadInt32 ()
        Array.init length f

    member inline r.ReadMap (f: int -> 'a * 'b): Map<'a, 'b> =
        let length = r.ReadInt32 ()
        Seq.init length f |> Map.ofSeq


type BinaryWriter with
    member inline w.WriteArray(xs: 'a array, f: 'a -> unit): unit =
        w.Write xs.Length
        for x in xs do f x

    member inline w.WriteMap(m: Map<'a, 'b>, f: 'a -> 'b -> unit): unit =
        w.Write m.Count
        for kv in m do f kv.Key kv.Value
