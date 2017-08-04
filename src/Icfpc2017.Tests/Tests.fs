namespace Icfpc2017.Tests

open NUnit.Framework
open System.IO

[<TestFixture>]
module Tests =
    [<Test>]
    let test_foo (): unit = 
        Assert.That("foo", Is.EqualTo("foo"))



[<TestFixture>]
module Deserialise = 
    [<Test>]
    let checkCanLoadSmallGame (): unit =
        let game =  """{"punter":0, "punters":2, "map":{"sites":[{"id":4},{"id":1},{"id":3},{"id":6},{"id":5},{"id":0},{"id":7},{"id":2}], "rivers":[{"source":3,"target":4},{"source":0,"target":1},{"source":2,"target":3}, {"source":1,"target":3},{"source":5,"target":6},{"source":4,"target":5}, {"source":3,"target":5},{"source":6,"target":7},{"source":5,"target":7}, {"source":1,"target":7},{"source":0,"target":7},{"source":1,"target":2}], "mines":[1,5]}}""" in
        let setup = 
            match ProtocolData.deserialize game with
            | ProtocolData.Setup setup -> setup
            | _ -> failwith ":("
        in
        let game = Game.initialState setup in
        begin
        Assert.That(game.Graph.NVerts, Is.EqualTo(8))
        Assert.That(game.Graph.Edges.Length, Is.EqualTo(12))
        end