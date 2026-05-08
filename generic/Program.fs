// For more information see https://aka.ms/fsharp-console-apps

open Generic

type Commands = 
| NuevoJuego
| GrabarJuego
| CargarJuego
| Salir

Menu.mostrar 
    10 
    15
    [|
        NuevoJuego,"New Game"
        Salir, "Exit"
    |] 
|> fun o ->
    printfn $"El usuario eligio: {o}"

