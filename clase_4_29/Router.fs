module App.Router

open Types
//
// La funcion de este modulo es decidir
// que se muestra en la pantalla
//

type RouterState =
| ShowingMenu
| ShowingRock
| ShowingMonster
| ShowingSaludo
| Terminated

let initialState = ShowingMenu

let rec mainLoop state =
    match state with 
    | ShowingMenu -> 
        match Menu.mostrar() with 
        | NewRockSim -> ShowingRock
        | NewMonsterSim -> ShowingMonster
        | NewSaludo -> ShowingSaludo
        | Exit -> Terminated
    | ShowingRock -> 
        Rock.mostrar()
        ShowingMenu
    | ShowingMonster ->
        Monster.mostrar()
        ShowingMenu
    | ShowingSaludo ->
        Saludo.mostrar()
        ShowingMenu
    | Terminated ->
        Terminated
    |> fun s ->
        if s <> Terminated then
            mainLoop s

let mostrar() =
    initialState
    |> mainLoop