module Pervasives

let (|Prefix|_|) (p:string) (s:string) =
    if s.StartsWith(p) then
        Some(s.Substring(p.Length))
    else
        None

let impossible<'a> : 'a = failwith "impossible"
