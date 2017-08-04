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
            serialize (Move (Claim { punter = 0; source = 0; target = 1 })),
            Is.EqualTo(@"{""punter"":0,""source"":0,""target"":1}"))
        Assert.That(
            serialize (Move (Pass { punter = 0 })),
            Is.EqualTo(@"{""punter"":0}"))

    [<Test>]
    let test_deserialize () : unit =
        Assert.That(
            deserialize @"{""you"": ""llama""}",
            Is.EqualTo(
                HandshakeAck {
                    you = "llama"
                }))
        Assert.That(
            deserialize @"{""punter"": 0, ""punters"": 1, map: {""sites"": [{""id"": 0}, {""id"": 1}], ""rivers"": [{""source"": 0, ""target"": 1}], ""mines"": [0, 1]}}",
            Is.EqualTo(
                Setup {
                    punter = 0
                    punters = 1
                    map =
                        {
                            sites = [| { id = 0 }; { id = 1 } |]
                            rivers = [| { source = 0; target = 1 } |]
                            mines = [| 0; 1 |]
                        }
                }))
        Assert.That(
            deserialize @"{""move"": {""moves"":[{""claim"":{""punter"":0,""source"":0,""target"":0}},{""pass"":{""punter"":0}}]}}",
            Is.EqualTo(
                RequestMove {
                    move =
                        {
                            moves =
                                [|
                                    Claim { punter = 0; source = 0; target = 0 }
                                    Pass { punter = 0 }
                                |]
                        }
                    }))
        Assert.That(
            deserialize @"{""stop"": {""moves"": [{""claim"":{""punter"":0,""source"":0,""target"":0}},{""pass"":{""punter"":0}}], ""scores"":[{""punter"": 0, ""score"": 42}]}}",
            Is.EqualTo(
                Stop {
                    stop =
                        {
                            moves =
                                [|
                                    Claim { punter = 0; source = 0; target = 0 }
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
