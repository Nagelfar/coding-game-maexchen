module Domain

open Logging
open Model

let appendDices round dice = { round with dices = dice :: round.dices }

let evolve state =
    function
    | RoundStarting -> { state with rounds = Round.intial :: state.rounds }
    | NewDices c ->
        let updatedRound = appendDices state.rounds.Head c
        { state with rounds = updatedRound :: state.rounds.Tail }
    | _ -> state

let isPasch toss =
    let (mfirst, mlast) = toss
    mfirst = mlast

let asNumber toss =
    let (mfirst, mlast) = toss
    mfirst * 10 + mlast

let isMyTossBetter (mine : Toss) (other : Toss) =
    let (mfirst, mlast) = mine
    let (ofirst, olast) = other
    let mMaexchen = mfirst = 2 && mlast = 1
    let oMaexchen = ofirst = 2 && olast = 1
    if (mMaexchen) then true
    else if (oMaexchen) then false
    else
        let mPasch = mfirst = mlast
        let oPasch = ofirst = olast
        if (mPasch && not oPasch) then true
        else
            let mNumber = asNumber mine
            let oNumber = asNumber other
            mNumber > oNumber

let lastRound state = List.tryHead state.rounds

let decideIfWeShouldLie (state : State) (toss : Toss) =
    let lastRound = lastRound state
    match lastRound with
    | Some(round) ->
        match List.tryHead round.dices with
        | Some(previousToss) ->
            let myTossIsBetter = isMyTossBetter toss previousToss
            if myTossIsBetter then SayDices(toss, state.self)
            else SayDices((5, 5), state.self)
        | None -> SayDices(toss, state.self)
    | None -> SayDices(toss, state.self)

let decideIfOtherPlayerLied (state : State) =
    let lastRound = lastRound state
    match lastRound with
    | Some(round) ->
        match List.tryHead round.dices with
        | Some(previousToss) ->
            if isPasch previousToss then See(state.self)
            else Roll(state.self)
        | None -> Roll(state.self)
    | None -> Roll(state.self)

//    See(state.self)
let decide (state : State) event : Command list =
    match event with
    | Attending -> [ Attend(state.self) ]
    | Dices x -> [ decideIfWeShouldLie state x ]
    | SeeOrRoll -> [ decideIfOtherPlayerLied (state) ]
    | _ ->
        log "warn" (sprintf "No handler for event %A defined" event)
        []
