module Sevenguis.History

type Model<'state> =
    { current: 'state
      undoHistory: list<'state>
      redoHistory: list<'state> }

let initialModel initialState =
    { current = initialState
      undoHistory = []
      redoHistory = [] }

type Message<'state> =
    | Undo
    | Redo
    | PushState of ('state -> 'state)
    | ReplaceState of ('state -> 'state)

let update message model =
    match message with
    | Undo ->
        match model.undoHistory with
        | [] -> model
        | newState :: newUndoHistory ->
            { current = newState
              undoHistory = newUndoHistory
              redoHistory = model.current :: model.redoHistory }
    | Redo ->
        match model.redoHistory with
        | [] -> model
        | newState :: newRedoHistory ->
            { current = newState
              undoHistory = model.current :: model.undoHistory
              redoHistory = newRedoHistory }
    | PushState updateState ->
        { model with
            current = updateState model.current
            undoHistory = model.current :: model.undoHistory
            redoHistory = [] }
    | ReplaceState updateState ->
        { model with current = updateState model.current }
