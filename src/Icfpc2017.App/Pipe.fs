module Pipe

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text

type T = private {
    InStream: Stream
    OutStream: Stream
}

let debug = ref true

let connect (host: string) (port: int32): T =
    let client = new TcpClient () in
    eprintf "Connecting on port %i... " port
    let t = 
        async {
            let! () = client.ConnectAsync(host, port) |> Async.AwaitTask            
            eprintf "OK!\n"
            let stream = client.GetStream ()
            return {InStream=stream; OutStream=stream}
        }
    in Async.RunSynchronously(t)

let std () = 
    {InStream=Console.OpenStandardInput ();
     OutStream=Console.OpenStandardOutput ()}

let close (p: T): unit = 
    p.InStream.Dispose ();
    p.OutStream.Dispose ()

let accept (server: TcpListener): Async<T> =
    async {
        let! sock = server.AcceptSocketAsync() |> Async.AwaitTask
        let stream = new NetworkStream(sock, true)
        return {InStream=stream; OutStream=stream}
    }

let inline retry f = 
    let rec go f = function
    | 0 -> failwith "<>-<"
    | n -> try f () with _ -> go f (n - 1)
    in go f 100

let commonRead (p: T) (deserialize : string -> 'a): 'a =
    let rec readLength (sb: StringBuilder) =
        match char (retry p.InStream.ReadByte) with
        | ':' -> int (sb.ToString ())
        | ch  ->
            ignore (sb.Append ch);
            readLength sb
    and readMessage (ob : byte array) offset =
        if offset = ob.Length
        then
            let message = deserialize (Encoding.ASCII.GetString ob)
            if !debug then eprintf "<<< %A\n" message
            message
        else
            let read = retry (fun () -> p.InStream.Read(ob, offset, ob.Length - offset))
            readMessage ob (offset + read)        
    in readMessage (readLength (StringBuilder ()) |> Array.zeroCreate) 0

let read (p: T): ProtocolData.MessageIn =
    commonRead p ProtocolData.deserialize
(* 
let serverRead (p: T): Async<ProtocolData.MessageOut> =
    commonRead p ProtocolData.serverDeserialize *)

let _write (stream: Stream) (b: byte array) =
    retry (fun () -> stream.Write(b, 0, b.Length))

let commonWrite (p: T) (serialize: 'a -> string) (message: 'a): unit = 
    if !debug then eprintf ">>> %A\n" message
    let input = serialize message
    let ib = Encoding.ASCII.GetBytes input
    sprintf "%d:" ib.Length |> Encoding.ASCII.GetBytes |> _write p.OutStream
    _write p.OutStream ib

let write (p: T) (message: ProtocolData.MessageOut): unit =
    commonWrite p ProtocolData.serialize message

(* let serverWrite (p: T) (message: ProtocolData.MessageIn): Async<unit> =
    commonWrite p ProtocolData.serverSerialize message *)
