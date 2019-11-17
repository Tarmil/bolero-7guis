module Sevenguis.Dialog

open Bolero
open Bolero.Html
open Elmish

type Config<'model, 'msg> =
    { closeMsg: 'msg
      title: 'model -> string
      body: 'model -> Dispatch<'msg> -> Node }

let view config model dispatch =
    div [attr.classes ["modal"; if Option.isSome model then "is-active"]] [
        div [
            attr.``class`` "modal-background"
            on.click (fun _ -> dispatch config.closeMsg)
        ] []
        div [attr.``class`` "modal-content"] [
            div [attr.``class`` "box has-text-centered"] [
                cond model <| function
                | None -> empty
                | Some model ->
                    concat [
                        h2 [attr.``class`` "subtitle"] [text (config.title model)]
                        div [] [config.body model dispatch]
                    ]
            ]
        ]
        button [
            "aria-label" => "close"
            attr.``class`` "modal-close is-large"
            on.click (fun _ -> dispatch config.closeMsg)
        ] [RawHtml "&times;"]
    ]
