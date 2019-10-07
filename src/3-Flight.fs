module Sevenguis.Flight

open System
open Bolero.Html

type Direction = OneWay | Return

type Model =
    { direction: Direction
      startDateText: string
      endDateText: string }

    member this.startDate =
        match DateTime.TryParse(this.startDateText) with
        | true, x -> Some x
        | false, _ -> None

    member this.endDate =
        match DateTime.TryParse(this.endDateText) with
        | true, x -> Some x
        | false, _ -> None

    member this.canBook =
        match this.direction with
        | OneWay ->
            Option.isSome this.startDate
        | Return ->
            match this.startDate, this.endDate with
            | Some startDate, Some endDate -> startDate < endDate
            | _ -> false

let initModel =
    let today = DateTime.Today.ToString("dd.MM.yyyy")
    { direction = OneWay; startDateText = today; endDateText = today }

type Message =
    | SetDirection of Direction
    | SetStartDateText of string
    | SetEndDateText of string
    | Book

let update message model =
    match message with
    | SetDirection direction -> { model with direction = direction }
    | SetStartDateText text -> { model with startDateText = text }
    | SetEndDateText text -> { model with endDateText = text }
    | Book ->
        printfn "Booking!"
        model // TODO

let view model dispatch =
    concat [
        select [
            on.change (fun e ->
                match e.Value :?> string with
                | "oneway" -> OneWay
                | _ -> Return
                |> SetDirection
                |> dispatch)
        ] [
            option [
                attr.value "oneway"
                attr.selected (model.direction = OneWay)
            ] [text "one-way flight"]
            option [
                attr.value "return"
                attr.selected (model.direction = Return)
            ] [text "return flight"]
        ]
        input [
            attr.classes [if Option.isNone model.startDate then "invalid"]
            bind.input model.startDateText (dispatch << SetStartDateText)
        ]
        input [
            attr.classes [if Option.isNone model.endDate then "invalid"]
            bind.input model.endDateText (dispatch << SetEndDateText)
            attr.disabled (model.direction = OneWay)
        ]
        button [
            attr.disabled (not model.canBook)
            on.click (fun _ -> dispatch Book)
        ] [text "Book"]
    ]
