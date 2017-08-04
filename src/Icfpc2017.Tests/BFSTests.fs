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
        let distances = Array2D.init 1 2 (fun i j -> -1) in
        ShortestPath.BFS graph 0u distances;
        Assert.That(distances.[0,1], Is.EqualTo(1))
        Assert.That(distances.[0,0], Is.EqualTo(0))

    [<Test>]
    let star_vertex_test (): unit =
        let nSites = 4u in
        let edges = [ (0u, 1u); (0u, 2u); (0u, 3u) ] in
        let mines = [| 0u |] in 
        let graph = Graph.create mines nSites edges in
        let distances = Array2D.init 1 4 (fun i j -> -1) in
        ShortestPath.BFS graph 0u distances;
        Assert.That(distances.[0,1], Is.EqualTo(1))
        Assert.That(distances.[0,2], Is.EqualTo(1))
        Assert.That(distances.[0,3], Is.EqualTo(1))
        Assert.That(distances.[0,0], Is.EqualTo(0))

    [<Test>]
    let long_worm (): unit =
        let nSites = 4u in
        let edges = [ (0u, 1u); (1u, 2u); (2u, 3u) ] in
        let mines = [| 3u |] in 
        let graph = Graph.create mines nSites edges in
        let distances = Array2D.init 4 4 (fun i j -> -1) in
        ShortestPath.BFS graph 3u distances;
        Assert.That(distances.[3,0], Is.EqualTo(3))
        Assert.That(distances.[3,1], Is.EqualTo(2))
        Assert.That(distances.[3,2], Is.EqualTo(1))
        Assert.That(distances.[3,3], Is.EqualTo(0))