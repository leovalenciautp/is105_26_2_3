module App.Saludo

open System
open System.Threading

open Utils

type ProgramState = 
| Running
| Terminated


type EntryState =
| AskingForData
| ShowingData



type State = {
    ProgramState: ProgramState
    Tick: int
    Clock: int
    RedrawScreen: bool
    EntryState: EntryState
    EntryX: int
    EntryY: int
    EntryData: string
    EntryLabel: string
}

let initialState = {
    ProgramState = Running
    Tick = -1
    Clock = 0
    RedrawScreen = true
    EntryState = AskingForData
    EntryX = 0
    EntryY = 15
    EntryData = ""
    EntryLabel = "Entra tu nombre: "
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

let updateEntryKeboard (key:ConsoleKeyInfo) state =
    if state.EntryState = AskingForData then 
        match key with
        | k when Char.IsLetter k.KeyChar ->
            {state with EntryData = state.EntryData+key.KeyChar.ToString(); RedrawScreen=true}
        | k ->
            match k.Key with 

            | ConsoleKey.Spacebar ->
                {state with EntryData = state.EntryData+key.KeyChar.ToString(); RedrawScreen=true}
            | ConsoleKey.Backspace ->
                {state with EntryData = state.EntryData.Remove(state.EntryData.Length-1,1); RedrawScreen = true}
            | ConsoleKey.Enter ->
                { state with EntryState = ShowingData;RedrawScreen=true}
            | _ -> state
    else
        state



let processKeyboard state =
    if Console.KeyAvailable then 
        let k = Console.ReadKey true
        state
        |> updateSaludoKeyboard k.Key
        |> updateEntryKeboard k
    else
        state

let redrawClock state =
    displayMessageRight 0 ConsoleColor.Yellow $"{state.Clock}"
    state


let redrawEntry state =
    match state.EntryState with 
    | AskingForData ->
        displayMessage state.EntryX state.EntryY ConsoleColor.Red state.EntryLabel
        displayMessage (state.EntryX+state.EntryLabel.Length) state.EntryY ConsoleColor.Blue state.EntryData
        displayMessage (state.EntryX+state.EntryLabel.Length+state.EntryData.Length) state.EntryY ConsoleColor.Red "☠️"
    | ShowingData ->
        displayMessage state.EntryX state.EntryY ConsoleColor.Cyan $"Hola {state.EntryData}"
    state

let redrawScreen state =
    if state.RedrawScreen then 
        Console.Clear()
        state
        |> redrawClock
        |> redrawEntry
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


let pipeline = [|
    updateTick
    updateClock
    processKeyboard
    redrawScreen
|]
let miLoop = createMainLoop pipeline (fun s -> s.ProgramState <> Terminated)

let mostrar() =
    Console.Clear()
    Console.CursorVisible <- false

    initialState 
    |> miLoop
    |> ignore

    Console.CursorVisible <- true
    Console.Clear()