[<AutoOpen>]
module Model

open Logging

type Token = Token of string

// type Player = Player of string

let Player = "fm"

type Rundennummer = Rundennummer of int

type Card =
    | Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace

type Hand = Card list

type Value = int

type Command =
    | Join of string
    | SetGeld of int * string
    | PickCard of string
    | Stay of string

type PlayerScore = string * int

type Event =
    | Ok
    | RoundStarting
    | Set
    | CardRecived of Card
    | BankRecived of Card
    | StayOrCard
    | Money of int
    | RoundEnded of PlayerScore list

let LowAceValue : Value = 1
let HighAceValue : Value = 11

type HandValue =
    { aceHigh : Value
      aceLow : Value }

let parseCard value =
    match value with
    | "2" -> Two
    | "3" -> Three
    | "4" -> Four
    | "5" -> Five
    | "6" -> Six
    | "7" -> Seven
    | "8" -> Eight
    | "9" -> Nine
    | "10" -> Ten
    | "11" -> Ace
    | _ -> failwithf "Unknown card value %s" value

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


// and GameStarting = { game: Token }
// | GameStarted of
let eventHandlers (event : Event) : Command list =
    match event with
    | CardRecived x -> [SetGeld(1,"S")]
    | OK -> [ Join("foo") ]
    | _ ->
        log "warn" (sprintf "No handler for event %A defined" event)
        []


let registerOnServer = Join(Player)