module Sevenguis.Temperature

open Bolero.Html

type Model =
    { celsius: float }

    member this.fahrenheit =
        this.celsius * 9. / 5. + 32.

let initModel =
    { celsius = 0. }

type Message =
    | SetCelsius of float
    | SetFahrenheit of float

let update message model =
    match message with
    | SetCelsius c ->
        { model with celsius = c }
    | SetFahrenheit f ->
        { model with celsius = (f - 32.) * 5. / 9. }

let view model dispatch =
    concat [
        input [bind.inputFloat model.celsius (dispatch << SetCelsius)]
        text " celsius = "
        input [bind.inputFloat model.fahrenheit (dispatch << SetFahrenheit)]
        text " fahrenheit"
    ]
