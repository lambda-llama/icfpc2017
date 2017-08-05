namespace Icfpc2017.Tests

open NUnit.Framework
open System.IO

open Graph
open ShortestPath
open FastUnion
open Union

[<TestFixture>]
module FastUnionTest =
    let vert id = {Id = id; IsSource=false; Coords=None}
    let mine id = {Id = id; IsSource=true; Coords=None}

    [<Test>]
    let unionTest () = 
        let union2 = {
            Mines = [3]
            Sites = []
        }
        let union1 = {
            Mines = []
            Sites = [0;1;2]
        }
        let d = [(3, [|1;2;3|])] |> Map.ofList
        Assert.That(Union.getScore union1 union2 d, Is.EqualTo(14))

    [<Test>]
    let singleEdge () = 
        let edges = [(0, 1)]
        let verts = [|mine 0; vert 1|]
        let graph = Graph.create verts edges
        let union = FastUnion.T graph
        let d = ShortestPath.BFS graph
        Assert.That(union.IsInSame 0 1, Is.EqualTo(false))
        union.Unite 0 1
        Assert.That(union.IsInSame 0 1, Is.EqualTo(true))


    [<Test>]
    let star () = 
        let edges = [(0, 1);(0,2);(0,3)]
        let verts = [|mine 3; vert 0; vert 1; vert 2;|]
        let graph = Graph.create verts edges
        let union = FastUnion.T graph
        Assert.That(union.TestGetUnion 0, Is.EqualTo({ Mines=[];Sites=[0]}))
        Assert.That(union.TestGetUnion 1, Is.EqualTo({ Mines=[];Sites=[1]}))
        Assert.That(union.TestGetUnion 2, Is.EqualTo({ Mines=[];Sites=[2]}))
        Assert.That(union.TestGetUnion 3, Is.EqualTo({ Mines=[3];Sites=[]}))

        let d = ShortestPath.Compute graph
        Assert.That(d.[3].[0], Is.EqualTo(1))
        Assert.That(d.[3].[1], Is.EqualTo(2))
        Assert.That(d.[3].[2], Is.EqualTo(2))

        Assert.That(union.IsInSame 0 1, Is.EqualTo(false))
        Assert.That(union.IsInSame 0 2, Is.EqualTo(false))
        Assert.That(union.IsInSame 0 3, Is.EqualTo(false))
        Assert.That(union.IsInSame 1 2, Is.EqualTo(false))
        Assert.That(union.IsInSame 1 3, Is.EqualTo(false))
        Assert.That(union.IsInSame 2 3, Is.EqualTo(false))

        Assert.That(union.GetIncrement(0, 1, d), Is.EqualTo(0))
        Assert.That(union.GetIncrement(0, 2, d), Is.EqualTo(0))
        Assert.That(union.GetIncrement(0, 3, d), Is.EqualTo(1))
        
        union.Unite 0 3

        Assert.That(union.IsInSame 0 1, Is.EqualTo(false))
        Assert.That(union.IsInSame 0 2, Is.EqualTo(false))
        Assert.That(union.IsInSame 0 3, Is.EqualTo(true))
        Assert.That(union.IsInSame 1 2, Is.EqualTo(false))
        Assert.That(union.IsInSame 1 3, Is.EqualTo(false))
        Assert.That(union.IsInSame 2 3, Is.EqualTo(false))
