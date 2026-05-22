//
// Ejemplo de un agente (mailbox processor) que implementa el Actor Model
// para coordinacion de threads en una aplicacion que es multi threaded.
//
// May 20 2026

//
// Erlang, es el lenguage por excelencia de las telecomunicaciones.
//

open System


type Misil = {
    X: int
    Y: int
}

type State = {
    Clock1: int
    Clock2: int
    AlienX: int
    AlienY: int
    Misiles: Misil list
}

type Mensaje =
| TempoUnoHizoClick
| TempoDosHizoClick
| RefreshScreen
| TeclaPresionada of ConsoleKey
| AnimarMisil

let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let x = Console.BufferWidth-msg.Length
    displayMessage x y color msg


let displayClock1 state =
    displayMessage 0 0 ConsoleColor.Red $"{state.Clock1}"

let displayClock2 state =
    displayMessageRight 0 ConsoleColor.Red $"{state.Clock2}"

let displayAlien state =
    displayMessage state.AlienX state.AlienY ConsoleColor.Yellow "👽"

let displayMisiles state =
    state.Misiles
    |> List.iter (fun misil -> 
        displayMessage misil.X misil.Y ConsoleColor.Yellow "=>"
    )
let refreshScreen state =
    Console.Clear()
    [|
        displayClock1
        displayClock2
        displayAlien
        displayMisiles
    |]
    |> Array.iter (fun f -> state |> f)
    
    state
    
let initialState = {
    Clock1 = 0
    Clock2 = 0
    AlienX = Console.BufferWidth/2
    AlienY = Console.BufferHeight/2
    Misiles = []
}

let procesarTecla key state =
    match key with 
        | ConsoleKey.UpArrow ->
            {state with AlienY = max 0 (state.AlienY-1)}
        | ConsoleKey.DownArrow ->
            {state with AlienY = min (Console.BufferHeight-1) (state.AlienY+1)}
        | ConsoleKey.LeftArrow ->
            {state with AlienX = max 0 (state.AlienX-1)}
        | ConsoleKey.RightArrow ->
            {state with AlienX = min (Console.BufferWidth-2) (state.AlienX+1)}
        | ConsoleKey.Spacebar ->
            let nuevoMisil = {
                X = state.AlienX+2
                Y = state.AlienY
            }
            {state with Misiles = nuevoMisil :: state.Misiles}
        | _ -> state


let actualizarMisiles state =
    if state.Misiles <> [] then 
        state.Misiles
        |> Seq.map (fun misil -> {misil with X=misil.X+1})
        |> Seq.filter (fun misil -> misil.X < Console.BufferWidth-2)
        |> Seq.toList
        |> fun nuevosMisiles ->
            {state with Misiles = nuevosMisiles} 
    else
        state
let updateState oldState message =
    match message with 
    | TempoUnoHizoClick ->
        {oldState with Clock1 = oldState.Clock1+1}
    | TempoDosHizoClick ->
        {oldState with Clock2 = oldState.Clock2+1}
    | RefreshScreen ->
        oldState |> refreshScreen
    | TeclaPresionada k ->
        oldState |> procesarTecla k
    | AnimarMisil ->
        oldState |> actualizarMisiles

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

let rec animar() =
    async {
        do! Async.Sleep 25
        buzon.Post AnimarMisil
        return! animar()
    }

let rec leerTeclado() =
    async {
        let salir = 
            Console.KeyAvailable &&
            let k = Console.ReadKey true
            buzon.Post (TeclaPresionada k.Key)
            k.Key = ConsoleKey.Escape
        if not salir then 
            do! Async.Sleep 10
            return! leerTeclado()
    }

Console.Clear()
Console.CursorVisible <- false

tempoUno() |> Async.StartImmediate
tempoDos() |> Async.StartImmediate
redibujar() |> Async.StartImmediate
animar() |> Async.StartImmediate

leerTeclado() |> Async.RunSynchronously

Console.CursorVisible <- true
Console.Clear()