namespace Icfpc2017.Tests

open NUnit.Framework
open System.IO

open Graphs

[<TestFixture>]
module TraversalTests =
    let vert id = {Id=V id; IsSource=false; Coords=None}
    let mine id = {Id=V id; IsSource=true; Coords=None}

    [<Test>]
    let singleEdge () =
        let edges = [|(V 0, V 1)|] in
        let verts = [|mine 0; vert 1|] in
        let graph = Graph.create verts edges in
        let source = Graph.sources graph |> Array.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)

    [<Test>]
    let star () =
        let edges = [|(V 0, V 1); (V 0, V 2); (V 0, V 3)|] in
        let verts = [|mine 0; vert 1; vert 2; vert 3|] in
        let graph = Graph.create verts edges in
        let source = Graph.sources graph |> Array.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[2], Is.EqualTo 1)
        Assert.That(distances.[3], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)

    [<Test>]
    let longWorm () =
        let edges = [|(V 0, V 1); (V 1, V 2); (V 2, V 3)|] in
        let verts = [|vert 0; vert 1; vert 2; mine 3|] in
        let graph = Graph.create verts edges in
        let source = Graph.sources graph |> Array.last
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances, Is.EqualTo [|3; 2; 1; 0|])

    let withGap () =
        let edges = [|(V 0, V 42)|] in
        let verts = [|mine 0; vert 42|] in
        let graph = Graph.create verts edges in
        let source = Graph.sources graph |> Array.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)
