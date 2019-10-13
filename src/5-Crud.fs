module Sevenguis.Crud

open Bolero.Html

type Person =
    { name: string
      surname: string }

type Model =
    { filter: string
      inputName: string
      inputSurname: string
      people: Person list
      selected: int option }

let initialModel =
    { filter = ""
      inputName = ""
      inputSurname = ""
      people = [
        { name = "Hans"; surname = "Emil" }
        { name = "Max"; surname = "Mustermann" }
        { name = "Roman"; surname = "Tisch" }
      ]
      selected = None }

type Message =
    | SetFilter of string
    | SetInputName of string
    | SetInputSurname of string
    | Select of int
    | Create
    | Update
    | Delete

module List =

    let replaceAt index value list =
        let before, after = List.splitAt index list
        before @ value :: List.tail after

    let removeAt index list =
        let before, after = List.splitAt index list
        before @ List.tail after

let update message model =
    match message with
    | SetFilter filter ->
        { model with filter = filter }
    | SetInputName name ->
        { model with inputName = name }
    | SetInputSurname surname ->
        { model with inputSurname = surname }
    | Select i ->
        match List.tryItem i model.people with
        | Some selectedPerson ->
            { model with
                selected = Some i
                inputName = selectedPerson.name
                inputSurname = selectedPerson.surname }
        | None ->
            { model with selected = None }
    | Create ->
        let newPerson =
            { name = model.inputName
              surname = model.inputSurname }
        { model with
            inputName = ""
            inputSurname = ""
            selected = None
            people = model.people @ [newPerson] }
    | Update ->
        match model.selected with
        | Some selected ->
            let newPerson =
                { name = model.inputName
                  surname = model.inputSurname }
            { model with
                people = List.replaceAt selected newPerson model.people }
        | None -> model
    | Delete ->
        match model.selected with
        | Some selected ->
            { model with
                selected = None
                people = List.removeAt selected model.people }
        | None -> model

let view model dispatch =
    concat [
        div [] [
            text "Filter prefix: "
            input [bind.input model.filter (dispatch << SetFilter)]
        ]
        div [attr.``class`` "columns"] [
            div [attr.``class`` "column"] [
                forEach (Seq.indexed model.people) <| fun (index, person) ->
                    let isSelected =
                        match model.selected with
                        | Some selected -> selected = index
                        | None -> false
                    div [
                        attr.classes [if isSelected then "selected"]
                        on.click (fun _ -> dispatch (Select index))
                    ] [textf "%s, %s" person.surname person.name]
            ]
            div [attr.``class`` "column"] [
                div [] [
                    text "Name: "
                    input [bind.input model.inputName (dispatch << SetInputName)]
                ]
                div [] [
                    text "Surname: "
                    input [bind.input model.inputSurname (dispatch << SetInputSurname)]
                ]
            ]
        ]
        div [] [
            button [on.click (fun _ -> dispatch Create)] [text "Create"]
            button [on.click (fun _ -> dispatch Update)] [text "Update"]
            button [on.click (fun _ -> dispatch Delete)] [text "Delete"]
        ]
    ]
