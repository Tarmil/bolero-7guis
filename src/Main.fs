module Sevenguis.Main

open Elmish
open Bolero

type Page =
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/temperature">] Temperature
    | [<EndPoint "/flight">] Flight
    | [<EndPoint "/timer">] Timer

type Model =
    { page: Page
      counter: Counter.Model
      temperature: Temperature.Model
      flight: Flight.Model
      timer: Timer.Model }

let initModel =
    { page = Counter
      counter = Counter.initModel
      temperature = Temperature.initModel
      flight = Flight.initModel
      timer = Timer.initModel }

type Message =
    | Goto of Page
    | Counter of Counter.Message
    | Temperature of Temperature.Message
    | Flight of Flight.Message
    | Timer of Timer.Message

let update message model =
    match message with
    | Message.Goto page ->
        { model with page = page }
    | Message.Counter message ->
        { model with counter = Counter.update message model.counter }
     | Message.Temperature message ->
        { model with temperature = Temperature.update message model.temperature }
    | Message.Flight message ->
        { model with flight = Flight.update message model.flight }
    | Message.Timer message ->
        { model with timer = Timer.update message model.timer }

let router = Router.infer Goto (fun m -> m.page)

let view model dispatch =
    match model.page with
    | Page.Counter -> Counter.view model.counter (dispatch << Message.Counter)
    | Page.Temperature -> Temperature.view model.temperature (dispatch << Message.Temperature)
    | Page.Flight -> Flight.view model.flight (dispatch << Message.Flight)
    | Page.Timer -> Timer.view model.timer (dispatch << Message.Timer)

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkSimple (fun _ -> initModel) update view
        |> Program.withRouter router
        |> Program.withSubscription (fun _ ->
            Timer.subscribe () |> Cmd.map Message.Timer
        )
