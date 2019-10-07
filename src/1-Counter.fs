module Sevenguis.Counter

open Bolero.Html

type Model =
    { value : int }

let initModel =
    { value = 0 }

type Message =
    | Increment

let update message model =
    match message with
    | Increment -> { model with value = model.value + 1 }

let view model dispatch =
    concat [
        input [attr.value (string model.value)]
        button [on.click (fun _ -> dispatch Increment)] [text "Count"]
    ]
