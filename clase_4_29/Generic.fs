module App.Generic


//
// Esto es una funcion generica
//
let leo (a:'leo) (b:'samuel) : 'samuel =
    printfn $"{a} {b}"
    b

let c = leo "Hola" "mundo"
let d = leo 4.0 "mundo"

let h = leo true false

//
// Records con funciones
//
type LeoFuncion = {
    f1 : int -> int // Signature, firma
    f2 : float ->float
    f3 :  int -> unit
    f4 : int -> float -> string -> unit
    tuberia: (int -> int) list
}

let leoF  x = 
    2.0*x
let leoInicial = {
    f1 = fun x -> x+1
    f2 = leoF
    f3 = fun x -> printfn $"{x}"
    f4 = fun (a:int) (b:float) (c:string) -> () 
    tuberia = [
        fun x -> 2*x
        fun x -> 1+x
        fun x -> x/4
    ]
}

leoInicial
|> fun r ->
    r.tuberia
    |> List.iter ( fun f -> 
        let r = f 3
        printfn $"Resultado es {r}"
        )


