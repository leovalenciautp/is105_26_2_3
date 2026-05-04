module App.Monster

open System
open System.Threading

open Utils

type ProgramState = 
| Running
| Terminated

type State = {
    ProgramState: ProgramState
    x: int
    y: int
    RedrawScreen: bool
}

let initialState = {
    ProgramState = Running
    x = Console.BufferWidth/2
    y = Console.BufferHeight/2
    RedrawScreen = true
}

let displayMonster state =
    displayMessage state.x state.y ConsoleColor.Yellow "👽"
    state

let updateMonsterKeyboard key state =
    let newState =
        match key with 
        | ConsoleKey.UpArrow -> {state with y= max 0 (state.y-1)}
        | ConsoleKey.DownArrow -> {state with y = min (Console.BufferHeight-1) (state.y+1)}
        | ConsoleKey.LeftArrow -> {state with x = max 0 (state.x-1)}
        | ConsoleKey.RightArrow -> {state with x = min (Console.BufferWidth-2) (state.x+1)}
        | ConsoleKey.Escape -> {state with ProgramState = Terminated}
        | _ -> state
    if state <> newState then
        {newState with RedrawScreen = true}
    else
        state 
let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state 
        |> updateMonsterKeyboard k.Key
    else
        state

let redrawScreen state =
    if state.RedrawScreen then 
        Console.Clear()
        state |> displayMonster
        |> fun s ->
            {s with RedrawScreen=false}
    else
        state

let rec mainLoop state =
    let newState =
        state
        |> processKeyboard
        |> redrawScreen
    if newState.ProgramState <> Terminated then 
        Thread.Sleep 25
        mainLoop newState

let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false

    initialState
    |> mainLoop

    Console.CursorVisible <- true
    Console.Clear()
