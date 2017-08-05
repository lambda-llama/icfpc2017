namespace Icfpc2017.Tests

open NUnit.Framework

open Game

[<TestFixture>]
module IndexTests =
    [<Test>]
    let eToI () = 
        let index = [|"foo"; "bar"; "boo"|] |> Index.create
        Assert.That(index.e(0), Is.EqualTo("foo"))
        Assert.That(index.e(1), Is.EqualTo("bar"))        
        Assert.That(index.e(2), Is.EqualTo("boo"))                

    [<Test>]
    let iToE () = 
        let index = [|"foo"; "bar"; "boo"|] |> Index.create
        Assert.That(index.i("foo"), Is.EqualTo(0))
        Assert.That(index.i("bar"), Is.EqualTo(1))        
        Assert.That(index.i("boo"), Is.EqualTo(2))    