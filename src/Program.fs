// Learn more about F# at http://fsharp.org
open System
open Logging
open Connectivity

let messageLoop receiver dispatch =
    let loop =
        fun () ->
            while true do
                try
                    receiver dispatch
                with
                    | e -> logex "warn" e

    let thread = System.Threading.Thread(loop)
    thread.IsBackground <- true
    thread.Start()

let sendMessageToServer sender command =
    match ServerCommunication.translate command with
    | Some(message) -> sender message
    | None -> log "warn" (sprintf "Not able to send command '%A'" command)

let receiveFromServer receiver handlers =
    let received = receiver()
    match ServerCommunication.interpret received with
    | Some(event) -> handlers (event)
    | None ->
        log "warn"
            (sprintf "Cannot interprete and handle server-event '%A'" received)

let handleEvents sendResponse event =
    let result = ApplicationServices.eventHandlers event
    logVerbose (sprintf "Responding to %A with %A" event result)
    for command in result do
        sendResponse command

[<EntryPoint>]
let main argv =
    let (rawSender, rawReceiver) = connect System.Net.IPAddress.Loopback 9000
    let sender = sendMessageToServer rawSender
    let receiver = receiveFromServer rawReceiver
    messageLoop receiver (handleEvents sender)
    sender ApplicationServices.registerOnServer
    Console.ReadLine() |> ignore
    0 // return an integer exit code
