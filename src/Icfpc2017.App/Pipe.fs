module Pipe

open System.Net
open System.Net.Sockets
open System.Text

type T = private {
    Stream: NetworkStream
}

let debug = ref true

let connect (host: string) (port: int32): Async<T> =
    let client = new TcpClient () in
    async {
        printf "Connecting on port %i... " port
        let! () =
            client.ConnectAsync(host, port)
            |> Async.AwaitTask
        printf "OK!\n"
        return {Stream=client.GetStream ()}
    }

let commonRead (p: T) (deserialize : string -> 'a): Async<'a> =
    let rec readLength (sb: StringBuilder) =
        match char (p.Stream.ReadByte ()) with
        | ':' -> int (sb.ToString ())
        | ch  ->
            ignore (sb.Append ch);
            readLength sb
    and readMessage (ob : byte array) offset =
        async {
            if offset = ob.Length
            then
                let message = deserialize(Encoding.ASCII.GetString ob)
                if !debug then printf "<<< %A\n" message
                return message
            else
                let! read =
                    p.Stream.ReadAsync(ob, offset, ob.Length - offset)
                    |> Async.AwaitTask
                return! readMessage ob (offset + read)
        }
    in readMessage (readLength (StringBuilder ()) |> Array.zeroCreate) 0

let read (p: T): Async<ProtocolData.MessageIn> =
    commonRead p ProtocolData.deserialize

let serverRead (p: T): Async<ProtocolData.MessageOut> =
    commonRead p ProtocolData.serverDeserialize

let _write (stream: NetworkStream) (b: byte array) =
    stream.WriteAsync(b, 0, b.Length) |> Async.AwaitTask

let commonWrite (p: T) (serialize: 'a -> string) (message: 'a): Async<unit> = async {
    if !debug then printf ">>> %A\n" message
    let input = serialize message
    let ib = Encoding.ASCII.GetBytes input
    let! _ = sprintf "%d:" ib.Length
            |> Encoding.ASCII.GetBytes
            |> _write p.Stream
    return! _write p.Stream ib
}

let write (p: T) (message: ProtocolData.MessageOut): Async<unit> =
    commonWrite p ProtocolData.serialize message

let serverWrite (p: T) (message: ProtocolData.MessageIn): Async<unit> =
    commonWrite p ProtocolData.serverSerialize message
