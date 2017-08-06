module Pervasives

open System.IO
open System.Diagnostics

(* SET TO false BEFORE FINAL SUBMISSION *)
let debug = ref true

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


type BinaryReader with
    member inline r.ReadArray(f: unit -> 'a): 'a array =
        let length = r.ReadInt32 ()
        Array.init length (fun _ -> f ())


type BinaryWriter with
    member inline w.WriteArray(xs: 'a array, f: 'a -> unit): unit =
        w.Write (Array.length xs)
        for x in xs do f x
