module ApplicationServices

open Model
open Domain
open Logging

let mutable private events : Event list = []
let buildState events = List.fold evolve State.initial events

let eventHandlers (event : Event) : Command list =
    logInfo (sprintf "Current Events %A" events)
    let state = buildState events
    logInfo (sprintf "Derived State %A" state)
    let decision = decide state event
    logInfo (sprintf "Decision %A" decision)
    events <- List.append events [ event ]
    decision


let registerOnServer = Join((buildState events).self)
