module App.Saludo

open System
open System.Threading

open Utils

type ProgramState = 
| Running
| Terminated


type KeyboardHelper =
| Inactive
| Active
| WithData

type KeyboardState = {
    KeyboardHelper: KeyboardHelper
    data: string
    x: int
    y: int
    CursorX: int
}

let initialKeyboard = {
    KeyboardHelper = Inactive
    data = ""
    x = 0
    y = 0
    CursorX = 0
}

type State = {
    ProgramState: ProgramState
    Tick: int
    Clock: int
    RedrawScreen: bool
    KeyboardState: KeyboardState
}

let initialState = {
    ProgramState = Running
    Tick = -1
    Clock = 0
    RedrawScreen = true
    KeyboardState = initialKeyboard
}

let updateTick state =
    {state with Tick = state.Tick+1}

let updateClock state =
    if state.Tick <> 0 && state.Tick % 40 = 0 then 
        {state with Clock=state.Clock+1;RedrawScreen=true}
    else
        state

let updateSaludoKeyboard key state =
    match key with 
    | ConsoleKey.Escape -> {state with ProgramState=Terminated}
    | _ -> state

let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> updateSaludoKeyboard k.Key
    else
        state

let redrawClock state =
    displayMessageRight 0 ConsoleColor.Yellow $"{state.Clock}"
    state

let redrawMensaje (state:State) =
    displayMessage 0 15 ConsoleColor.Cyan "Entra tu nombre: "
    let helper = {
        KeyboardHelper = Active
        data = ""
        x = 18
        y = 15
        CursorX = 18
    }
    {state with KeyboardState = helper}
let redrawScreen state =
    if state.RedrawScreen then 
        Console.Clear()
        state
        |> redrawClock
        |> redrawMensaje
        |> fun s ->        
        {s with RedrawScreen = false}
    else
        state

let rec mainLoop state =
    let newState =
        state
        |> updateTick
        |> updateClock
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