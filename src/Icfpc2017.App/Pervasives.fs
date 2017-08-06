module Pervasives

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
