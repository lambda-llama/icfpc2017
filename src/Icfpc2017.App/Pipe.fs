module Pipe

open System.Net
open System.Net.Sockets
open System.Text

type T = private {
    Stream: NetworkStream
}

let connect (port: int32): Async<T> =
    let client = new TcpClient () in
    async {
        printf "Connecting on port %i... " port
        let! () =
            client.ConnectAsync("punter.inf.ed.ac.uk", port)
            |> Async.AwaitTask
        printf "OK!\n"
        return {Stream=client.GetStream ()}
    }

let read (p: T): Async<ProtocolData.MessageIn> =
    let rec readLength (sb: StringBuilder) =
        match char (p.Stream.ReadByte ()) with
        | ':' -> int (sb.ToString ())
        | ch  ->
            ignore (sb.Append ch);
            readLength sb
    and readMessage (ob : byte array) offset =
        async {
            if offset = ob.Length
            then return ProtocolData.deserialize(Encoding.ASCII.GetString ob)
            else
                let! read =
                    p.Stream.ReadAsync(ob, offset, ob.Length - offset)
                    |> Async.AwaitTask
                return! readMessage ob (offset + read)
        }
    in readMessage (readLength (StringBuilder ()) |> Array.zeroCreate) 0

let _write (stream: NetworkStream) (b: byte array) =
    stream.WriteAsync(b, 0, b.Length) |> Async.AwaitTask

let write (p: T) (input: string): Async<unit> = async {
    let ib = Encoding.ASCII.GetBytes input
    let! _ = sprintf "%d:" ib.Length
            |> Encoding.ASCII.GetBytes
            |> _write p.Stream
    return! _write p.Stream ib
}
