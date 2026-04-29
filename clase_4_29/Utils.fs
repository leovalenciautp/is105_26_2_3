module App.Utils

open System
let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let start = Console.BufferWidth-msg.Length
    displayMessage start y color msg