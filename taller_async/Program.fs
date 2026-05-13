open System
open System.Threading


let displayMessage x y color (msg:string) =
    Console.SetCursorPosition(x,y)
    Console.ForegroundColor <- color
    msg |> Console.Write

let displayMessageRight y color (msg:string) =
    let start = Console.BufferWidth-msg.Length
    displayMessage start y color msg
let saludar() = 
    async {
        do! Async.Sleep 5000
        displayMessage 0 0 ConsoleColor.Blue "Hola Mundo!"
    }

// saludar() |> Async.StartImmediate
// let rec loop() =
//     Thread.Sleep 25
//     loop()

// loop()

let rec reloj x =
    async {
        displayMessageRight 0 ConsoleColor.Yellow $"{x}"
        do! Async.Sleep 1000
        return! reloj (x+1)
    }

let rec leerTecla() =
    async {
        if Console.KeyAvailable then 
            let k = Console.ReadKey true
            displayMessage 0 10 ConsoleColor.Cyan "                      "
            displayMessage 0 10 ConsoleColor.Cyan $"Tecla: {k.Key}"
        return! leerTecla()
    }

let f x = 
    async {
        do! Async.Sleep 5000
        return 2*x
    }

let g x = 
    async {
        do! Async.Sleep 4000
        return 3*x
    }

let calcular() =
    async {
        let! p1 = f 2 |> Async.StartChild
        let! p2 = g 3 |> Async.StartChild
        let! r1 = p1
        let! r2 = p2
        let r = r1+r2
        displayMessage 0 12 ConsoleColor.Red $"Resultado: {r}"
    }

//
// Async.Start fuerza a la funcion a usar el Thread Pool
//
leerTecla() |> Async.Start
//
// Asyn.StartImmediate
// Arranca la fucion en el mismo hilo que la llama
//
saludar() |> Async.Start

calcular() |> Async.Start


Console.Clear()
Console.CursorVisible <- false
// Aqui el thread principal espera la funcion
reloj 0 |> Async.RunSynchronously 

//
// En general este programa sufre de un Race Condition
//
// Por esta razon toda la I/O del UI sucede en un solo Thread
// Se le llama el UI Thread, or el Thread principal.
// 
