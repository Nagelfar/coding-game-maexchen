module Domain

open Logging
open Model

let appendDices round dice = { round with dices = dice :: round.dices }

let evolve state =
    function
    | Ok -> state
    | RoundStarting -> { state with rounds = Round.intial :: state.rounds }
    | Dices c ->
        let updatedRound = appendDices state.rounds.Head c
        { state with rounds = updatedRound :: state.rounds.Tail }
    | _ -> state

let decide (state : State) event : Command list =
    match event with
    | Dices x -> [ Roll(state.self) ]
    | Attending -> [ Attend(state.self) ]
    | _ ->
        log "warn" (sprintf "No handler for event %A defined" event)
        []
