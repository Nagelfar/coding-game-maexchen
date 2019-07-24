module ApplicationServices
open Model
open Domain

open Logging

let mutable private events : Event list = []

let private buildState events = List.fold evolve State.initial events

let eventHandlers (event : Event) : Command list =
    let state = buildState events
    logInfo (sprintf "State %A from events %A" state events)

    let decision = decide state event

    logInfo (sprintf "Decision %A" decision)
    
    events <- List.append events [ event ]
    decision

let registerOnServer = Join((buildState events).self)
