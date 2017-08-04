namespace Icfpc2017.Tests

open NUnit.Framework
open System.IO


[<TestFixture>]
module BFS = 
    let vert id = {Graph.Id = id; Graph.IsSource = false; Graph.Coords = None}
    let mine id = {Graph.Id = id; Graph.IsSource = true; Graph.Coords = None}

    [<Test>]
    let two_vertex_test (): unit =
        let nSites = 2u in
        let edges = [ (0u, 1u) ] in
        let verts = [| mine 0u; vert 1u |] in
        let graph = Graph.create verts edges in
        let distances = Array.create 4 -1 in        
        ShortestPath.BFS graph 0u distances;
        Assert.That(distances.[1], Is.EqualTo(1))
        Assert.That(distances.[0], Is.EqualTo(0))

    [<Test>]
    let star_vertex_test (): unit =
        let nSites = 4u in
        let edges = [ (0u, 1u); (0u, 2u); (0u, 3u) ] in
        let verts = [| mine 0u; vert 1u; vert 2u; vert 3u |] in 
        let graph = Graph.create verts edges in
        let distances = Array.create 4 -1 in
        ShortestPath.BFS graph 0u distances;
        Assert.That(distances.[1], Is.EqualTo(1))
        Assert.That(distances.[2], Is.EqualTo(1))
        Assert.That(distances.[3], Is.EqualTo(1))
        Assert.That(distances.[0], Is.EqualTo(0))

    [<Test>]
    let long_worm (): unit =
        let nSites = 4u in
        let edges = [ (0u, 1u); (1u, 2u); (2u, 3u) ] in
        let verts = [| vert 0u; vert 1u; vert 2u; mine 3u |] in 
        let graph = Graph.create verts edges in
        let distances = Array.create 4 -1 in
        ShortestPath.BFS graph 3u distances;
        Assert.That(distances.[0], Is.EqualTo(3))
        Assert.That(distances.[1], Is.EqualTo(2))
        Assert.That(distances.[2], Is.EqualTo(1))
        Assert.That(distances.[3], Is.EqualTo(0))
