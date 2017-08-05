namespace Icfpc2017.Tests

open NUnit.Framework
open System.IO

open Graphs

[<TestFixture>]
module TraversalTests =
    let id = {IsSource=false; Coords=None}
    let mine id = {IsSource=true; Coords=None}

    [<Test>]
    let singleEdge () =
        let edges = [|(0, 1)|] in
        let nVertices = 2 in
        let graph = Graph.create nVertices [|0|] edges in
        let source = Graph.sources graph |> Array.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)

    [<Test>]
    let star () =
        let edges = [|(0, 1); (0, 2); (0, 3)|] in
        let nVertices = 4 in
        let graph = Graph.create nVertices [|0|] edges in
        let source = Graph.sources graph |> Array.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[2], Is.EqualTo 1)
        Assert.That(distances.[3], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)

    [<Test>]
    let longWorm () =
        let edges = [|(0, 1); (1, 2); (2, 3)|] in
        let nVertices = 4 in
        let graph = Graph.create nVertices [|3|] edges in
        let source = Graph.sources graph |> Array.last
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances, Is.EqualTo [|3; 2; 1; 0|])
