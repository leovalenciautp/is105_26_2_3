module App.Menu

open System
open System.Threading

//
// Esta linea es para traer los simbolos
// del module App.Utils
//
open App.Utils
open App.Types

type MenuState =
| Active
| Terminated


type State = {
    MenuState: MenuState
    X: int
    Y: int
    CurSorSelection: int
    CursorX: int
    Commands: (Command * string) array
    RedrawScreen: bool
}

let initialState = {
    MenuState = Active
    X = 20
    Y = 10
    CurSorSelection = 0
    CursorX = 18
    Commands = [|
        NewRockSim,"Simulacion de Roca"
        NewMonsterSim, "Simulacion de Monstruo"
        Exit,"Salir"
    |]
    RedrawScreen = true
}

let drawMenu state =
    state.Commands
    |> Array.iteri (fun i (_,legend) ->
        displayMessage state.X (state.Y+i) ConsoleColor.Cyan legend
    )

    displayMessage state.CursorX (state.Y+state.CurSorSelection) ConsoleColor.Yellow "*"
let redrawScreen state =
    if state.RedrawScreen then
        Console.Clear()
        state |> drawMenu
        {state with RedrawScreen = false}
    else
        state

let updateMenuKeyboard key state =
    let newState =
        match key with 
        | ConsoleKey.UpArrow -> {state with CurSorSelection = max 0 (state.CurSorSelection-1)}
        | ConsoleKey.DownArrow -> {state with CurSorSelection = min (state.Commands.Length-1) (state.CurSorSelection+1)}
        | ConsoleKey.Enter -> {state with MenuState = Terminated}
        | _ -> state

    if newState <> state then 
        {newState with RedrawScreen = true}
    else
        state

let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> updateMenuKeyboard k.Key
    else
        state
let rec mainLoop state =
    let newState = 
        state
        |> processKeyboard
        |> redrawScreen
    if newState.MenuState = Active then
        Thread.Sleep 25
        mainLoop newState
    else
        state

let mostrar() =
    let oldForeground = Console.ForegroundColor
    Console.CursorVisible <- false

    let state =
        initialState
        |> mainLoop

    Console.CursorVisible <- true
    Console.ForegroundColor <- oldForeground
    Console.Clear()
    fst state.Commands[state.CurSorSelection]