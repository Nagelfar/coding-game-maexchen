module ServerCommunication

open Logging

let translate (command : Command) =
    let tokenToString (Token t) = t
    match command with
    | Join (Player p) -> Some("JOIN;" + p)
    | SetGeld(betrag, p) -> Some("SET;" + (sprintf "%A" betrag) + ";" + p)
    | Stay (Player p) -> Some("STAY;" + p)
    | PickCard p -> Some("CARD;" + p)
    // | JoinGame g -> Some("JOIN;"+ tokenToString g.game )
    | _ ->
        log "warn" (sprintf "Could not translate command %A" command)
        None

let private cleanup (message : string) =
    let cleaned = message.Trim()
    if(cleaned <> message) then
        logVerbose (sprintf "Cleaned %A to %A" message cleaned)
    cleaned

let private parseCard value =
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

let private parse (message : string) =
    let parts = message.Split [| ';' |]
    match parts with
    | [| "OK" |] -> Some(Ok)
    | [| "ROUND STARTING" |] -> Some(RoundStarting)
    | [| "SET" |] -> Some(Set)
    | [| "CARD"; card |] -> Some(CardRecived(parseCard card))
    | [| "BANK"; card |] -> Some(BankRecived(parseCard card))
    | [| "STAY_OR_CARD" |] -> Some(StayOrCard)
    | [| "MONEY"; money |] -> Some(Money(money |> int))
    // | [|"ROUND ENDED";playerString|] ->
    //     let players = playerString.Split
    //     Some(Money(money |> int))
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
