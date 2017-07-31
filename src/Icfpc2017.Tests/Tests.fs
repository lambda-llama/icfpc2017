namespace Icfpc2017.Tests

open NUnit.Framework

[<TestFixture>]
module Tests =
    [<Test>]
    let llama_says_lambda (): unit =
        Assert.That(Llama.say, Is.EqualTo("λ"))
