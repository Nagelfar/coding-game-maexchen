module Connectivity

open System
open System.Net
open System.Net.Sockets
open System.Text
open Logging

type ServerMessage = string

type ServerResonse = string

type Socket =
    { Endpoint : IPEndPoint
      Client : UdpClient }

type Sender = ServerMessage -> unit

type Receiver = unit -> ServerResonse

type ServerConnection = Sender * Receiver

let initialize (ip : IPAddress) port =
    let socket = { 
        Endpoint = new IPEndPoint(ip, port)
        Client = new UdpClient() }

    socket.Client.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, true)
    socket.Client.Connect(socket.Endpoint)

    socket

let encode (message : string) = Encoding.Default.GetBytes(message)
let decode (data : byte []) = Encoding.Default.GetString(data)

let receive (socket : Socket) : ServerResonse =
    let data = socket.Client.Receive(ref socket.Endpoint)
    let decoded = decode (data)
    logVerbose (sprintf "Reveived %A" decoded)
    decoded

let send (socket : Socket) (message : ServerMessage) =
    logVerbose (sprintf "Sending %A" message)
    let data = encode message
    // socket.Client.Send(data, data.Length, socket.Endpoint) |> ignore
    socket.Client.Send(data, data.Length) |> ignore

let connect ip port : ServerConnection =
    let socket = initialize ip port
    let sender = send socket
    let receiver = fun () -> receive socket
    (sender, receiver)
