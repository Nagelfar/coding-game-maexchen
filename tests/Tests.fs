module Tests

open Model
open Domain
open ApplicationServices
open Xunit

[<Fact>]
let ``OK Leads to JOIN``() =
    let previousEvents = []
    let event = Attending
    let expected = Attend(State.initial.self)
    let decision = decide (ApplicationServices.buildState previousEvents) event
    Assert.Contains(expected, decision)
