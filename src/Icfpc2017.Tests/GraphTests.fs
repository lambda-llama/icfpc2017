namespace Icfpc2017.Tests

open NUnit.Framework

open Graphs

module GraphTests =
    [<Test>]
    let edgeId () =
        let edges = [|(0, 1)|]
        let graph = Graph.testCreate 2 [|0|] edges
        Assert.That(Graph.edgeId graph (0, 1), Is.EqualTo 0)
        Assert.That(Graph.edgeId graph (1, 0), Is.EqualTo 0)

module TraversalTests =
    [<Test>]
    let singleEdge () =
        let edges = [|(0, 1)|] in
        let graph = Graph.testCreate 2 [|0|] edges in
        let source = Graph.sources graph |> Seq.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)

    [<Test>]
    let star () =
        let edges = [|(0, 1); (0, 2); (0, 3)|] in
        let graph = Graph.testCreate 4 [|0|] edges in
        let source = Graph.sources graph |> Seq.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[2], Is.EqualTo 1)
        Assert.That(distances.[3], Is.EqualTo 1)
        Assert.That(distances.[0], Is.EqualTo 0)

    [<Test>]
    let longWorm () =
        let edges = [|(0, 1); (1, 2); (2, 3)|] in
        let graph = Graph.testCreate 4 [|3|] edges in
        let source = Graph.sources graph |> Seq.last
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances, Is.EqualTo [|3; 2; 1; 0|])

    [<Test>]
    let triangle () =
        let edges = [|(0, 1); (1, 2); (2, 0)|] in
        let graph = Graph.testCreate 3 [|0|] edges in
        let source = Graph.sources graph |> Seq.head
        let distances = Traversal.shortestPath graph source in
        Assert.That(distances.[0], Is.EqualTo 0)
        Assert.That(distances.[1], Is.EqualTo 1)
        Assert.That(distances.[2], Is.EqualTo 1)
