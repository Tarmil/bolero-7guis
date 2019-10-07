module Sevenguis.Timer

open System
open Bolero.Html
open Elmish

type Model =
    { duration: float
      elapsed: float
      started: DateTime }

let initModel =
    { duration = 10.
      elapsed = 0.
      started = DateTime.MinValue }

type Message =
    | Tick of DateTime
    | SetDuration of float
    | Reset

let update message model =
    match message with
    | Tick now ->
        let elapsed = (now - model.started).TotalSeconds
        { model with elapsed = min model.duration elapsed }
    | SetDuration duration ->
        { model with
            duration = duration
            elapsed = min duration model.elapsed }
    | Reset ->
        { model with elapsed = 0.; started = DateTime.Now }

let view model dispatch =
    concat [
        progress [
            attr.value model.elapsed
            attr.max model.duration
        ] []
        br []
        textf "%.1f / %.1f" model.elapsed model.duration
        br []
        input [
            attr.``type`` "range"
            attr.min 1.
            attr.max 100.
            attr.step 0.1
            bind.inputFloat model.duration (dispatch << SetDuration)
        ]
        br []
        button [on.click (fun _ -> dispatch Reset)] [text "Reset"]
    ]

let subscribe () =
    Cmd.ofSub <| fun dispatch ->
        let timer = new Timers.Timer(100.)
        timer.Elapsed.Add(fun args -> dispatch (Tick args.SignalTime))
        dispatch Reset
        timer.Start()
