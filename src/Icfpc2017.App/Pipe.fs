module Pipe

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text

open Pervasives

type T = private {
    InStream: Stream
    OutStream: Stream
}

let connect (host: string) (port: int32): T =
    let client = new TcpClient () in
    let t =
        async {
            eprintf "Connecting on port %i... " port
            let! () = client.ConnectAsync(host, port) |> Async.AwaitTask
            eprintf "OK!\n"
            let stream = client.GetStream ()
            return {InStream=stream; OutStream=stream}
        }
    Async.RunSynchronously(t)

let std () =
    {InStream=Console.OpenStandardInput ();
     OutStream=Console.OpenStandardOutput ()}

let close (p: T): unit =
    p.InStream.Dispose ();
    p.OutStream.Dispose ()


let read (p: T): ProtocolData.MessageIn =
    let rec readLength (sb: StringBuilder) =
        match char (p.InStream.ReadByte ()) with
        | ':' -> int (sb.ToString ())
        | ch  ->
            ignore (sb.Append ch);
            readLength sb
    and readMessage (ob : byte array) offset =
        if offset = ob.Length
        then
            let message = ProtocolData.deserialize (Encoding.ASCII.GetString ob)
            if !debug then eprintf "<<< %A\n" message
            message
        else
            let read = p.InStream.Read(ob, offset, ob.Length - offset)
            readMessage ob (offset + read)
    in readMessage (readLength (StringBuilder ()) |> Array.zeroCreate) 0

let _write (stream: Stream) (b: byte array) =
    stream.Write(b, 0, b.Length)

let write (p: T) (message: ProtocolData.MessageOut): unit =
    if !debug then eprintf ">>> %A\n" message
    let input = ProtocolData.serialize message
    let ib = Encoding.ASCII.GetBytes input
    sprintf "%d:" ib.Length |> Encoding.ASCII.GetBytes |> _write p.OutStream
    _write p.OutStream ib
