namespace Icfpc2017.Tests

open NUnit.Framework

open Pervasives

module PervasivesTests =
    [<Test>]
    let pairwise () =
        Assert.That (pairwise [1], Is.EqualTo [])
        Assert.That (pairwise [1; 2], Is.EqualTo [(1, 2)])
        Assert.That (pairwise [1; 2; 3], Is.EqualTo [(1, 2); (2, 3)])
        Assert.That (pairwise [1; 2; 3; 4], Is.EqualTo [(1, 2); (2, 3); (3, 4)])