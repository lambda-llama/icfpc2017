namespace Icfpc2017.Tests

open NUnit.Framework
open System.IO


[<TestFixture>]
module BFS = 
    [<Test>]
    let two_vertex_test (): unit =
        let nSites = 2u in
        let edges = [ (0u, 1u) ] in
        let mines = [| 0u |] in 
        let graph = Graph.create mines nSites edges in
        let distances = Array2D.zeroCreate 1 2 in
        ShortestPath.BFS graph 0u distances;
        Assert.That(1, Is.EqualTo(distances))