namespace Icfpc2017.Tests

open NUnit.Framework
open ProtocolData

[<TestFixture>]
module SerializationTests =
    [<Test>]
    let test_serialize () : unit =
        Assert.That(
            serialize (Handshake { me = "llama" }),
            Is.EqualTo(@"{""me"":""llama""}"))
        Assert.That(
            serialize (Ready {ready = 0; state = None; futures=[|{source = 0u; target = 1u}|]}),
            Is.EqualTo(@"{""ready"":0,""futures"":[{""source"":0,""target"":1}]}"))
        Assert.That(
            serialize (Move {move=Claim { punter = 0; source = 0u; target = 1u }; state=None}),
            Is.EqualTo(@"{""claim"":{""punter"":0,""source"":0,""target"":1}}"))
        Assert.That(
            serialize (Move {move=Pass { punter = 0 }; state=None}),
            Is.EqualTo(@"{""pass"":{""punter"":0}}"))
     
    [<Test>]
    let test_deserialize () : unit =
        Assert.That(
            deserialize @"{""you"": ""llama""}",
            Is.EqualTo(
                HandshakeAck {
                    you = "llama"
                }))
        Assert.That(
            deserialize @"{""punter"": 0, ""punters"": 1, map: {""sites"": [{""id"": 0}, {""id"": 1, ""x"": -1.5, ""y"": 0.5}], ""rivers"": [{""source"": 0, ""target"": 1}], ""mines"": [0, 1]}, ""settings"": {""futures"": true}}",
            Is.EqualTo(
                Setup {
                    punter = 0
                    punters = 1
                    map =
                        {
                            sites = [| { id = 0u; coords = None }; { id = 1u; coords = Some ({ x = -1.5; y = 0.5 }) } |]
                            rivers = [| { source = 0u; target = 1u } |]
                            mines = [| 0u; 1u |]
                        }
                    settings = { futures = true }
                }))
        Assert.That(
            deserialize @"{""punter"": 0, ""punters"": 1, map: {""sites"": [], ""rivers"": [], ""mines"": []}}",
            Is.EqualTo(
                Setup {
                    punter = 0
                    punters = 1
                    map =
                        {
                            sites = [||]
                            rivers = [||]
                            mines = [||]
                        }
                    settings = { futures = false }
                }))
        Assert.That(
            deserialize @"{""move"": {""moves"":[{""claim"":{""punter"":0,""source"":0,""target"":0}},{""pass"":{""punter"":0}}]}}",
            Is.EqualTo(
                RequestMove {
                    move =
                        {
                            moves =
                                [|
                                    Claim { punter = 0; source = 0u; target = 0u }
                                    Pass { punter = 0 }
                                |]
                        }
                    state = None
                }))
        Assert.That(
            deserialize @"{""stop"": {""moves"": [{""claim"":{""punter"":0,""source"":0,""target"":0}},{""pass"":{""punter"":0}}], ""scores"":[{""punter"": 0, ""score"": 42}]}}",
            Is.EqualTo(
                Stop {
                    stop =
                        {
                            moves =
                                [|
                                    Claim { punter = 0; source = 0u; target = 0u }
                                    Pass { punter = 0 }
                                |]
                            scores =
                                [|
                                    { punter = 0; score = 42 }
                                |]
                        }                    
                    }))
        Assert.That(
            deserialize @"{""timeout"": 42}",
            Is.EqualTo(
                Timeout {
                    timeout = 42
                }))
