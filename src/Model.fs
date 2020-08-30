[<AutoOpen>]
module Model

type Player = Player of string

type Dice = int
type Toss = Dice * Dice
type Roundnumber = int
type Score = int

type Command =
    | Join of Player
    | Attend of Player
    | SayDices of Toss * Player
    | See of Player
    | Roll of Player



type PlayerScore = Player * Score

type Event =
    | Ok
    | Rejected
    | Attending
    | RoundStarting
    | Dices of Toss
    | NewDices of Toss
    | SeeOrRoll
    | RoundEnded of PlayerScore list

type Round =
    { dices : Toss list }
    static member intial = { dices = [] }

type State =
    { self : Player
      players : Player list
      rounds : Round list }
    static member initial =
        { self = Player("fm")
          players = []
          rounds = [] }
