//
// Ejemplo de un agente (mailbox processor) que implementa el Actor Model
// para coordinacion de threads en una aplicacion que es multi threaded.
//
// May 20 2026

//
// Erlang, es el lenguage por excelencia de las telecomunicaciones.
//

open System


type State = {
    Clock1: int
    Clock2: int
}

type Mensaje =
| TempoUnoHizoClick
| TempoDosHizoClick
| RefreshScreen

let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let x = Console.BufferWidth-msg.Length
    displayMessage x y color msg


let refreshScreen state =
    Console.Clear()
    displayMessage 0 0 ConsoleColor.Red $"{state.Clock1}"
    displayMessageRight 0 ConsoleColor.Red $"{state.Clock2}"
    state
    
let initialState = {
    Clock1 = 0
    Clock2 = 0
}



let updateState oldState message =
    match message with 
    | TempoUnoHizoClick ->
        {oldState with Clock1 = oldState.Clock1+1}
    | TempoDosHizoClick ->
        {oldState with Clock2 = oldState.Clock2+1}
    | RefreshScreen ->
        oldState |> refreshScreen

//
// Para enviar y recibir mensajes
// necesitamos un MailBox Processor
//

let buzon = MailboxProcessor.Start (fun bandeja -> 
    let rec loop state =
        async {
            let! mensaje = bandeja.Receive()
            let newState = mensaje |> updateState state
            return! loop newState
        }
    initialState |> loop
)

let rec tempoUno() =
    async {
        do! Async.Sleep 250
        buzon.Post TempoUnoHizoClick
        return! tempoUno()
    }

let rec tempoDos () =
    async {
        do! Async.Sleep 500
        buzon.Post TempoDosHizoClick
        return! tempoDos()
    }

let rec redibujar() =
    async {
        buzon.Post RefreshScreen
        do! Async.Sleep 40
        return! redibujar()
    }

Console.Clear()
Console.CursorVisible <- false

tempoUno() |> Async.StartImmediate
tempoDos() |> Async.StartImmediate
redibujar() |> Async.RunSynchronously