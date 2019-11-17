module Sevenguis.Circles

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop
open Bolero
open Bolero.Html

type Circle =
    { x: int
      y: int
      radius: int }

type State =
    { circles: Circle list
      selected: int option }

type Model =
    { state: History.Model<State>
      dialog: Circle option }

let initialModel =
    { state = History.initialModel { circles = []; selected = None }
      dialog = None }

type Message =
    | ClickCanvas of x: int * y: int
    | DblClickCanvas of x: int * y: int
    | HistoryMsg of History.Message<State>
    | SetModalRadius of int
    | CloseModal

let contains x y circle =
    let dx = x - circle.x
    let dy = y - circle.y
    dx*dx + dy*dy < circle.radius*circle.radius

let updateHistory message model =
    { model with state = History.update message model.state }

let update message model =
    match message with
    | ClickCanvas (x, y) ->
        match model.state.current.circles |> List.tryFindIndexBack (contains x y) with
        | None ->
            let circle = { x = x; y = y; radius = 20 }
            updateHistory (History.PushState (fun s -> { circles = circle :: s.circles; selected = Some 0 })) model
        | Some selected ->
            updateHistory (History.ReplaceState (fun s -> { s with selected = Some selected })) model
    | DblClickCanvas (x, y) ->
        let selected = model.state.current.circles |> List.tryFindBack (contains x y)
        { model with dialog = selected }
    | HistoryMsg msg -> updateHistory msg model
    | SetModalRadius radius ->
        match model.dialog with
        | None -> model
        | Some circle -> { model with dialog = Some { circle with radius = radius } }
    | CloseModal ->
        match model.dialog, model.state.current.selected with
        | Some c, Some i ->
            { model with dialog = None }
            |> updateHistory (History.PushState (fun s -> { s with circles = List.replaceAt i c s.circles }))
        | _ -> model

type Component() =
    inherit ElmishComponent<State, Message>()

    let svgRef = ElementReferenceBinder()

    [<Inject>]
    member val JS = Unchecked.defaultof<IJSRuntime> with get, set

    member this.WithSvgPos f =
        async {
            let! svgPos = this.JS.InvokeAsync<float[]>("circles.getPos", svgRef.Ref).AsTask() |> Async.AwaitTask
            f svgPos.[0] svgPos.[1]
        }
        |> Async.Start

    override this.View state dispatch =
        svg [
            attr.bindRef svgRef
            attr.``class`` "circles"
            on.mousedown <| fun args ->
                this.WithSvgPos <| fun svgX svgY ->
                    dispatch (ClickCanvas (int (args.ClientX - svgX), int (args.ClientY - svgY)))
            on.dblclick <| fun args ->
                this.WithSvgPos <| fun svgX svgY ->
                    dispatch (DblClickCanvas (int (args.ClientX - svgX), int (args.ClientY - svgY)))
        ] [
            forEach (Seq.indexed state.circles) <| fun (index, circle) ->
                elt "circle" [
                    "cx" => circle.x
                    "cy" => circle.y
                    "r" => circle.radius
                    "stroke" => "black"
                    "stroke-width" => 2
                    "fill" =>
                        match state.selected with
                        | Some selected when index = selected -> "lightgray"
                        | _ -> "white"
                ] []
        ]

let modalCfg : Dialog.Config<Circle, Message> =
    { closeMsg = CloseModal
      title = fun circle ->
        sprintf "Adjust radius of circle at (%i, %i)" circle.x circle.y
      body = fun circle dispatch ->
        input [
            attr.``type`` "range"
            bind.changeInt circle.radius (dispatch << SetModalRadius)
        ] }

let view model dispatch =
    concat [
        button [
            on.click (fun _ -> dispatch (HistoryMsg History.Undo))
            attr.disabled (List.isEmpty model.state.undoHistory)
        ] [text "Undo"]
        button [
            on.click (fun _ -> dispatch (HistoryMsg History.Redo))
            attr.disabled (List.isEmpty model.state.redoHistory)
        ] [text "Redo"]
        ecomp<Component, _, _> [] model.state.current dispatch
        Dialog.view modalCfg model.dialog dispatch
    ]
