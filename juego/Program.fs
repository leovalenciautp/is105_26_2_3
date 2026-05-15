//
// Juego from scratch
//

open System
open System.Threading
open App.Utils

type ProgramState =
| Running
| Terminated

type SpriteState =
| Alive
| Hit

type Misil = {
    X: int
    Y: int
}

type State = {
    ProgramState: ProgramState
    AlienX: int
    AlienY: int
    AlienState: SpriteState
    RedibujarPantalla: bool
    Tick: int
    Misiles: Misil list
    EnemigoX: int
    EnemigoY: int
    EnemigoDir: int
    EnemigoEstado: SpriteState
    MisilesEnemigos: Misil list
    ColisionAlien: int
    ColisionEnemigo: int
}

let estadoInicial = {
    ProgramState = Running
    AlienX = Console.BufferWidth/2
    AlienY = Console.BufferHeight/2
    AlienState = Alive
    RedibujarPantalla = true
    Tick = -1
    Misiles = []
    EnemigoX = Console.BufferWidth-2
    EnemigoY = 0
    EnemigoDir = 1
    EnemigoEstado = Alive
    MisilesEnemigos = []
    ColisionAlien = 0
    ColisionEnemigo = 0
}

let dibujarAlien state =
    let sprite =
        if state.AlienState = Alive then 
            "👽"
        else
            "💥"
    mostrarMensaje state.AlienX state.AlienY ConsoleColor.Yellow sprite

let dibujarEnemigo state =
    let sprite =
        if state.EnemigoEstado = Alive then 
            "👾"
        else
            "💥"
    mostrarMensaje state.EnemigoX state.EnemigoY ConsoleColor.Yellow sprite

let dibujarMisiles state =
    state.Misiles
    |> List.iter ( fun misil ->
        mostrarMensaje misil.X misil.Y ConsoleColor.Yellow "=>" )

let dibujarMisilesEnemigos state =
    state.MisilesEnemigos
    |> List.iter ( fun misil ->
        mostrarMensaje misil.X misil.Y ConsoleColor.Red "<=" )


let redibujarPantalla state =
    if state.RedibujarPantalla then 
        Console.Clear()
        [|
            dibujarAlien
            dibujarMisiles
            dibujarEnemigo
            dibujarMisilesEnemigos
        |]
        |> Array.iter (fun f -> f state)
        {state with RedibujarPantalla=false}
    else
        state

let actualizarTick state =
    {state with Tick = state.Tick+1}

let actualizarMisiles state =
    if state.Misiles <> [] then 
        state.Misiles
        |> Seq.map (fun misil -> {misil with X=misil.X+1})
        |> Seq.filter (fun misil -> misil.X < Console.BufferWidth-2)
        |> Seq.toList
        |> fun nuevosMisiles ->
            {state with Misiles = nuevosMisiles;RedibujarPantalla=true} 
    else
        state

let actualizarMisilesEnemigos state =
    if state.MisilesEnemigos <> [] then 
        state.MisilesEnemigos
        |> Seq.map (fun misil -> {misil with X=misil.X-1})
        |> Seq.filter (fun misil -> misil.X >= 0)
        |> Seq.toList
        |> fun nuevosMisiles ->
            {state with MisilesEnemigos = nuevosMisiles;RedibujarPantalla=true} 
    else
        state

let actualizarDisparoEnemigo state =
    if state.EnemigoEstado = Alive then 
        if state.Tick % 10 = 0 then 
            let nuevoMisil = {
                X = state.EnemigoX-2
                Y = state.EnemigoY
            }
            {state with MisilesEnemigos= nuevoMisil :: state.MisilesEnemigos; RedibujarPantalla=true}
        else
            state
    else
        state
let actualizarEnemigo state =
    if state.EnemigoEstado = Alive then 
        if state.Tick % 4 = 0 then 
            let nuevaY = state.EnemigoY+state.EnemigoDir
            match nuevaY with 
            | y when y > Console.BufferHeight-1 -> Console.BufferHeight-1,-1
            | y when y < 0 -> 0,1
            | y -> y, state.EnemigoDir
            |> fun (y,dir) ->
                {state with EnemigoY=y;EnemigoDir=dir;RedibujarPantalla=true}
        else
            state
    else
        state


let detectarColisionConAlien state =
    state.MisilesEnemigos
    |> List.filter (fun misil -> not (misil.X = state.AlienX+1 && misil.Y = state.AlienY))
    |> fun nuevosMisiles ->
        if nuevosMisiles.Length <> state.MisilesEnemigos.Length then 
            {state with 
                AlienState=Hit
                MisilesEnemigos=nuevosMisiles
                RedibujarPantalla=true
                ColisionAlien=state.Tick
            }
        else
            state
let detectarColisionConEnemigo state =
    state.Misiles
    |> List.filter (fun misil -> not (misil.X = state.EnemigoX-1 && misil.Y = state.EnemigoY))
    |> fun nuevosMisiles ->
        if nuevosMisiles.Length <> state.Misiles.Length then 
            {state with 
                EnemigoEstado=Hit
                Misiles=nuevosMisiles
                RedibujarPantalla=true
                ColisionEnemigo=state.Tick
            }
        else
            state

let resetAlien state =
    if state.AlienState = Hit then 
        let tiempo = state.Tick-state.ColisionAlien
        if tiempo >= 160 then 
            {state with AlienState=Alive;RedibujarPantalla=true}
        else
            state
    else
        state

let resetEnemigo state =
    if state.EnemigoEstado = Hit then 
        let tiempo = state.Tick-state.ColisionEnemigo
        if tiempo >= 160 then 
            {state with EnemigoEstado=Alive;RedibujarPantalla=true}
        else
            state
    else
        state
let procesarTecladoApp key state =
    match key with 
    | ConsoleKey.Escape ->
        {state with ProgramState = Terminated}
    | _ -> state
let procesarTecladoAlien key state =
    if state.AlienState = Alive then 
        match key with 
        | ConsoleKey.Spacebar ->
            let nuevoMisil = {
                X = state.AlienX+2
                Y = state.AlienY
            }
            {state with Misiles = nuevoMisil :: state.Misiles}
        | ConsoleKey.UpArrow ->
            {state with AlienY = max 0 (state.AlienY-1)}
        | ConsoleKey.DownArrow ->
            {state with AlienY = min (Console.BufferHeight-1) (state.AlienY+1)}
        | ConsoleKey.LeftArrow ->
            {state with AlienX = max 0 (state.AlienX-1)}
        | ConsoleKey.RightArrow ->
            {state with AlienX = min (Console.BufferWidth-2) (state.AlienX+1)}
        | _ -> state
        |> fun nuevoEstado ->
            if nuevoEstado <> state then 
                {nuevoEstado with RedibujarPantalla=true}
            else
                state
    else
        state

let procesarTeclado state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> procesarTecladoApp k.Key
        |> procesarTecladoAlien k.Key
    else
        state

let rec mainLoop state =
    state
    |> actualizarTick
    |> actualizarMisiles
    |> actualizarEnemigo
    |> actualizarDisparoEnemigo
    |> actualizarMisilesEnemigos
    |> detectarColisionConAlien
    |> detectarColisionConEnemigo
    |> resetAlien
    |> resetEnemigo
    |> procesarTeclado
    |> redibujarPantalla
    |> fun nuevoEstado ->
        if nuevoEstado.ProgramState <> Terminated then
            Thread.Sleep 25
            nuevoEstado |> mainLoop

Console.Clear()
Console.CursorVisible <- false

estadoInicial
|> mainLoop

Console.Clear()
Console.CursorVisible <- true


