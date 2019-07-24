module Domain

open Logging
open Model

let computeCardValue aceSelector card =
    match card with
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 10
    | Queen -> 10
    | King -> 10
    | Ace -> aceSelector

let computeCard card =
    let aceHigh = computeCardValue HighAceValue
    let aceLow = computeCardValue LowAceValue
    { aceHigh = aceHigh card
      aceLow = aceLow card }

let addCard value card =
    let cardValue = computeCard card

    let sum =
        { aceHigh = value.aceHigh + cardValue.aceHigh
          aceLow = value.aceLow + cardValue.aceLow }
    sum

let computeHand (hand : Hand) =
    let initialSum =
        { aceHigh = 0
          aceLow = 0 }

    let handValue = List.fold addCard initialSum hand
    handValue

let evolve state =
    function
    | Ok -> { state with rounds = [] }
    | _ -> state

let decide (state : State) event : Command list =
    match event with
    | CardRecived x -> [ SetGeld(1, "S") ]
    | Ok -> [ Join(state.self) ]
    | Money x -> [ Stay(state.self) ]
    | _ ->
        log "warn" (sprintf "No handler for event %A defined" event)
        []
