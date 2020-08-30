module ServerCommunication

open Logging

let convertDice d = sprintf "%A" d

let parseDice (v : string) =
    let first = v.Substring(0, 1)
    let second = v.Substring(1, 1)
    (int first, int second)

let translate (command : Command) =
    match command with
    | Join(Player p) -> Some("JOIN;" + p)
    | Attend(Player p) -> Some("ATTEND;" + p)
    | SayDices((f,s), Player p) -> Some("DICES;" + (convertDice f) + "" + (convertDice s) + ";" + p)
    | See(Player p) -> Some("SEE;" + p)
    | Roll(Player p) -> Some("ROLL;" + p)
    | _ ->
        log "warn" (sprintf "Could not translate command %A" command)
        None

let private cleanup (message : string) =
    let cleaned = message.Trim()
    if (cleaned <> message) then
        logVerbose (sprintf "Cleaned %A to %A" message cleaned)
    cleaned

let private parse (message : string) =
    let parts = message.Split [| ';' |]
    match parts with
    | [| "OK" |] -> Some(Ok)
    | [| "REJECTED" |] -> Some(Rejected)
    | [| "ATTENDING" |] -> Some(Attending)
    | [| "ROUND STARTING" |] -> Some(RoundStarting)
    | [| "DICES"; dices |] -> Some(Dices(parseDice dices))
    | [| "NEW DICES"; dices |] -> Some(NewDices(parseDice dices))
    | [| "SEE OR ROLL" |] -> Some(SeeOrRoll)
    //     | [|"ROUND ENDED"; round; playerString|] ->
    //         let players = playerString.Split
    //         Some(Money(money |> int))
    // | [|"ROUND STARTED";rundennummer;players|] ->
    // let runde =  Rundennummer(rundennummer |>  int)
    // let playerList = players.Split [|','|] |> Array.toList |> List.map Player
    // Some(RoundStarted(runde, playerList ))
    | unmatched ->
        log "warn" (sprintf "Could not interpret message %A" unmatched)
        None

let interpret (message : string) =
    let cleaned = cleanup message
    parse cleaned
