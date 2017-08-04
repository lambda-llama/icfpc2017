namespace Icfpc2017.Tests

open NUnit.Framework

[<TestFixture>]
module Tests =
    [<Test>]
    let test_foo (): unit = 
        Assert.That("foo", Is.EqualTo("foo"))
