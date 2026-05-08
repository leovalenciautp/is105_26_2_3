module App.Utils

open System
open System.Threading
let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let start = Console.BufferWidth-msg.Length
    displayMessage start y color msg


let createMainLoop (pipeline: ('state -> 'state) array)  (isRunning: ('state -> bool)) =
    let rec mainLoop (state:'state) =
        pipeline
        |> Array.fold (fun acc f -> f acc) state
        |> fun newState ->
            if isRunning newState then 
                Thread.Sleep 25
                newState |> mainLoop
            else
                newState
    
    mainLoop


let createMainLoop2 pipeline  isRunning =
    let rec mainLoop (state:'state) =
        pipeline
        |> Array.fold (fun acc f -> f acc) state
        |> fun newState ->
            if isRunning newState then 
                Thread.Sleep 25
                newState |> mainLoop
            else
                newState
    
    mainLoop