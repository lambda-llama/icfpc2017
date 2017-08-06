namespace Icfpc2017.Tests

open NUnit.Framework

open Game

module IndexTests =
    [<Test>]
    let eToI () =
        let index = [|"foo"; "bar"; "boo"|] |> Index.create
        Assert.That(index.e(0), Is.EqualTo "foo")
        Assert.That(index.e(1), Is.EqualTo "bar")
        Assert.That(index.e(2), Is.EqualTo "boo")

    [<Test>]
    let iToE () =
        let index = [|"foo"; "bar"; "boo"|] |> Index.create
        Assert.That(index.i("foo"), Is.EqualTo 0)
        Assert.That(index.i("bar"), Is.EqualTo 1)
        Assert.That(index.i("boo"), Is.EqualTo 2)

module StateTests =
    [<Test>]
    let serializeDeserialize () =
        let setup: ProtocolData.SetupIn = {
            punter = 0
            punters = 1
            map =
                {
                    sites = [| { id = 0u; coords = None }; { id = 1u; coords = None } |]
                    rivers = [| { source = 0u; target = 1u } |]
                    mines = [| 0u; 1u |]
                }
            settings = { futures = true }
        }

        let state = initialState setup <| Map.ofList [("foo", "bar")]
        Assert.That(state.Serialize () |> State.Deserialize, Is.EqualTo state)