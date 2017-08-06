namespace Icfpc2017.Tests

open NUnit.Framework

open Graphs

module GraphTests =
    [<Test>]
    let edgeId () =
        let graph = Graph.testCreate 2 [|0|] [|(0, 1)|]
        Assert.That(Graph.edgeId graph (0, 1), Is.EqualTo 0)
        Assert.That(Graph.edgeId graph (1, 0), Is.EqualTo 0)

    [<Test>]
    let sourcesSinks () =
        let graph = Graph.testCreate 4 [|0; 2|] [|(0, 1); (1, 3)|]
        Assert.That (Graph.sources graph, Is.EqualTo <| seq [0; 2])
        Assert.That (Graph.sinks graph, Is.EqualTo <| seq [1; 3])

    [<Test>]
    let nEdges () =
        let edges = [|(0, 1); (1, 3)|]
        let graph = Graph.testCreate 4 [|0; 2|] edges
        Assert.That (Graph.nEdges graph, Is.EqualTo edges.Length)

    [<Test>]
    let claiming () =
        let graph = Graph.testCreate 4 [|0; 2|] [|(0, 1); (1, 3)|]
        let edge = Graph.unclaimed graph |> Seq.head
        Assert.False (Graph.isClaimed graph edge)
        Assert.False (Graph.isClaimedBy 42 graph edge)
        Assert.That (Graph.edgeColor graph edge, Is.EqualTo None)

        let graph2 = Graph.claimEdge graph 42 (Edge.id edge)
        Assert.True (Graph.isClaimed graph2 edge)
        Assert.True (Graph.isClaimedBy 42 graph2 edge)
        Assert.That (Graph.claimedBy graph2 42, Is.EqualTo <| seq [edge])

    [<Test>]
    let adjacent () =
        let graph = Graph.testCreate 4 [|0; 2|] [|(0, 1); (0, 2); (1, 3)|]
        Assert.That (Graph.adjacent graph 0, Is.EqualTo <| seq [1; 2])

    [<Test>]
    let connectedComponents () =
        let edges =
            [|[|(0, 1); (1, 2); (0, 2)|];
              [|(3, 4)|];
              [|(5, 6)|]|]
        let graph = Graph.testCreate 7 [|0; 3|] (Array.concat edges)

        let components = Traversal.connectedComponents graph |> Seq.toArray
        Assert.That (components.Length, Is.EqualTo 2)
        Assert.That (Graph.unclaimed components.[0] |> Seq.map Edge.ends,
                     Is.EqualTo edges.[0])
        Assert.That (Graph.unclaimed components.[1] |> Seq.map Edge.ends,
                     Is.EqualTo edges.[1])

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
