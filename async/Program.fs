//
// Programacion Asíncrona
// (Asynchronous Programming)
// May 11 2026
//

//
// https://fable.io/repl
//

//
// Este codigo solo compile en Fable en linea,
// o si tienen instalado Fable en dotnet.
//
open Browser.Dom
open Browser.Types
open Fable.Core.JsInterop

let f x =
    async { //Computation Expression
        do! Async.Sleep 5000
        return 2*x
    }

//
// Hagamos que g se demore 4 segundos en completar
//
let g x =
    async {
        do! Async.Sleep 4000
        return 3*x
    }

//
// Por lo tanto principal se va a demorar 10 segundos
// en completar
//
let principal() =
    async {
        let! p1 = f 5 |> Async.StartChild
        let! p2 = g 4 |> Async.StartChild
        let! r1 = p1
        let! r2 = p2
        return r1+r2
    }


let resultado = document.getElementById "resultado"
let mostrar() =
    async {
        let! r = principal()
        resultado.textContent <- r.ToString()
    }

let header = document.getElementById "leo"

let rec clock t =
    async {
        header.textContent <- t.ToString()
        do! Async.Sleep 1000
        return! clock (t+1)
    }

let c2 = document.getElementById "clock2"
let rec clock2 t =
    async {
        c2.textContent <- t.ToString()
        do! Async.Sleep 250
        return! clock2 (t+1)
    }

let mutable buffer = ""
let mutable keypressed = false

let procesarTeclado (evento: Event) =
    let k = evento :?> KeyboardEvent
    buffer <- k.code
    keypressed <- true

let readKey() =
    async {
        while not keypressed do
            do! Async.Sleep 10
        keypressed <- false
        return buffer    
    }

document.addEventListener("keydown", procesarTeclado)


let canvas = document.getElementById "canvas" :?> HTMLCanvasElement


let dibujarCuadrado x y =
    let ctx = canvas.getContext_2d()
    ctx.fillStyle <- !^"rgb(0 200 0)"
    ctx.fillRect(x,y,50.0,50.0)

let borrarCuadrado() =
    let ctx = canvas.getContext_2d()
    ctx.clearRect(0.0,0.0,ctx.canvas.width,ctx.canvas.height)


let rec moverObjecto x (y:float) =
    async {
        let! k = readKey()
        match k with 
        | "ArrowRight" -> 
            borrarCuadrado()
            dibujarCuadrado (x+5.0) y
            return! moverObjecto (x+5.0) y
        | _ -> ()
    }

clock 0 |> Async.StartImmediate
clock2 0 |> Async.StartImmediate
mostrar() |> Async.StartImmediate
dibujarCuadrado 10.0 10.0
moverObjecto 10.0 10.0 |> Async.StartImmediate

//

// HTML
//

<html>
    <body>
        <h2 id="leo"></h2>
        <h2 id="clock2"<h2>
        <h3>Calculando:</h3>
        <p id="resultado"></p>
        <canvas id=canvas></canvas>

    </body>
</html>