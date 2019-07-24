[<AutoOpen>]
module Model

type Token = Token of string

type Player = Player of string

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
    | Join of Player
    | SetGeld of int * string
    | PickCard of string
    | Stay of Player

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

type Round =
    { dices : string list }

type State =
    { self : Player
      players : Player list
      rounds : Round list }
    static member initial =
        { self = Player("fm")
          players = []
          rounds = [] }
