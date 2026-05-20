//
// Ejemplo de un agente (mailbox processor) que implementa el Actor Model
// para coordinacion de threads en una aplicacion que es multi threaded.
//
// May 20 2026

//
// Erlang, es el lenguage por excelencia de las telecomunicaciones.
//

open System


let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let x = Console.BufferWidth-msg.Length
    displayMessage x y color msg

let rec tempoUno t =
    async {
        do! Async.Sleep 250
        displayMessage 0 0 ConsoleColor.Red $"{t}"
        return! tempoUno (t+1)
    }

let rec tempoDos t =
    async {
        do! Async.Sleep 500
        displayMessageRight 0 ConsoleColor.Red $"{t}"
        return! tempoUno (t+1)
    }

Console.Clear()
Console.CursorVisible <- false

tempoUno 0 |> Async.StartImmediate
tempoDos 0 |> Async.RunSynchronously