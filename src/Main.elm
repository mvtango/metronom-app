port module Main exposing (..)

import Browser

import Html
import Html exposing (Html, a, button, br, text, section, div, span, h1, h2, img, input, p )
import Html.Attributes exposing (autofocus, checked, class, placeholder, style, type_, value, readonly, disabled )
import Html.Events exposing (onClick, onInput, onSubmit)
import String
import Time
import Array
import Tuple

---- MODEL ----


type Msg
    = UpdateMode Mode
    | AddTodo Action
    | RemoveTodo Int
    | Tick Time.Posix
    | Noop


type Mode 
    = Stopped
    | Recording
    | Looping

type Action
    = Strike
    | In
    | Out
    | Pause


type alias Todo =
    { action : Action
    , tick : Int
    , tickEnd: Int
    }


type alias Model =
    { 
      todos : List Todo
    , mode: Mode
    , tick: Int
    , loop: Int
    , inside: Bool
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( Model [ ]
            Stopped 
            0 
            50 
            False
    , Cmd.none
    )


---- UPDATE ----

addPause : List Todo -> Int -> List Todo
addPause todos tick =
    let 
      last     = List.head (List.reverse todos)
      newTodo  = Todo Pause (case last of 
                                        Nothing -> 0
                                        Just todo -> case todo.action of
                                                     Pause -> (.tick todo)
                                                     _     -> (.tickEnd todo)+1) tick
    in
      case last of
          Nothing -> [ newTodo ]
          Just todo -> if todo.action == Pause then
                         (List.take (List.length todos-1) todos) ++ [ newTodo ]
                       else
                         todos ++ [ newTodo ]



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddTodo action ->
            let
                newTodos =
                    addPause model.todos model.tick ++ [ 
                                     Todo action (model.tick+1) (model.tick+2) 
                                   ]
                newInside =
                    if action == In then True else if action == Out then False else model.inside 
            in
            ( { model | todos = newTodos, inside = newInside, tick = model.tick + 3 }
             , saveTodos newTodos
            )

        RemoveTodo index ->
            let
                beforeTodos =
                    List.take index model.todos

                afterTodos =
                    List.drop (index + 1) model.todos

                newTodos =
                    beforeTodos ++ afterTodos
            in
            ( { model | todos = newTodos }, saveTodos newTodos )


        
        Tick time -> ( case model.mode of
                       Stopped   -> (model, Cmd.none)
                       Recording -> ({ model | tick = model.tick +1 }, Cmd.none )
                       Looping   -> ({ model | tick = remainderBy model.loop (model.tick + 1) }, Cmd.none)
                     )


        UpdateMode newmode -> case newmode of
                  Looping   -> ({ model | mode = newmode, loop = (if model.mode == Recording then model.tick else model.loop), tick = 0 }, Cmd.none)
                  Stopped   -> ({ model |   mode = newmode
                                          , todos = (if model.mode == Recording then 
                                                        addPause model.todos model.tick 
                                                    else
                                                        model.todos
                                                    )
                                          , loop = (if model.tick > model.loop then model.tick else model.loop) 
                                          , tick = -1
                                }, Cmd.none)
                  Recording -> ({ model | mode = newmode, tick = model.loop }, Cmd.none)

        Noop -> (model, Cmd.none)



---- VIEW ----

viewButton : Mode -> Bool -> Html Msg
viewButton mode off =
    case mode of
        Recording -> button [ onClick (UpdateMode mode), class "button is-primary", disabled off ] [
                               span [ class "icon" ] 
                                  [
                                    Html.i [ class "fas fa-plus" ] []
                                  ]
                            ]
        Stopped   -> button [ onClick (UpdateMode mode), class "button is-primary", disabled off ] [
                               span [ class "icon" ] 
                                  [
                                    Html.i [ class "fas fa-stop" ] []
                                  ]
                            ]
        Looping   -> button [ onClick (UpdateMode mode), class "button is-primary", disabled off ] [
                               span [ class "icon" ] 
                                  [
                                    Html.i [ class "fas fa-play" ] []
                                  ]
                            ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div [ ]
        [
        case model.mode of
          Stopped ->
                     span [] [
                          viewButton Looping False
                        , viewButton Stopped True
                        , viewButton Recording False
                    ]
          Looping ->
                     span [] [
                          viewButton Looping True
                        , viewButton Stopped False
                        , viewButton Recording True
                    ]
          Recording ->
                     span [] [
                          viewButton Looping True
                        , viewButton Stopped False
                        , viewButton Recording True
                     ]
        ]

viewHeader: Model -> Html Msg
viewHeader model =
        div [ class "level" ] [
            div [ class "level-left"] [
                  div [ class "level-item"] [
                        text ""
                  ]
                , div [ class "level-item"] 
                [
                 viewToolbar model
                ]
                , div [ class "level-item" ] [
                 div [    class ""
                       , style "visibility" (if model.mode == Recording then "visible" else "hidden") 
                     ] 
                  [
                         viewAction Strike True
                       , (if model.inside then viewAction Out True else text "")
                       , (if not model.inside then viewAction In True else text "")

                  ]
                ]
            ]
            ,div [ class "level-right"] [
            ]
        ]

type TickPosition  
        = BeforeP Int
        | CurrentP Int
        | AfterP Int
        | Nowhere

isCurrent: Int -> Int -> Todo -> TickPosition
isCurrent tick index todo =
    let 
        p = todo.tick - tick
        pa = abs(p)
        pr = pa < 10
    in
        if pa < 1 then CurrentP index  
        else if p>0 then BeforeP index else Nowhere

getCurrent: Model -> Maybe TickPosition 
getCurrent model =
    let
       current = List.filter (\ip -> case ip of
                                     Nowhere -> False
                                     _ -> True)
                 (List.indexedMap (isCurrent model.tick) model.todos)
    in
       List.head current


viewAction: Action -> Bool -> Html Msg
viewAction action create =
    let
        clickaction = onClick (if create then AddTodo action else Noop)
    in
        case action of
        Strike ->
            button [ class "button is-small is-danger align-baseline", clickaction ] [
                   span [ class "icon" ]
                      [
                        Html.i [ class "fas fa-slash" ] []
                      ]
            ]
        Out -> 
            button [ class "button is-small is-white align-baseline", clickaction ] [
                   span [ class "icon" ]
                      [
                        Html.i [ class "fas fa-sign-out-alt" ] []
                      ]
            ]
        In -> 
            button [ class "button is-small is-black align-baseline", clickaction ] [
                   span [ class "icon" ]
                      [
                        Html.i [ class "fas fa-sign-in-alt" ] []
                      ]
            ]
        Pause ->
            span [ class "action-pause" ] [ text "\u{00A0}" ]




view : Model -> Html Msg
view model =
    let 
        current = getCurrent model
    in
    div [ class "section", style "max-width" "50%", style "margin-left" "25%" ] [
          viewHeader model 
          ,div [ class "box", style "line-height" "2.1em", style "text-align" "left"] 
                 (List.map (viewTodo model.tick) (if model.mode == Recording 
                                                  then addPause model.todos model.tick 
                                                  else model.todos))

        ]
                 
        


viewTodo : Int -> Todo -> Html Msg
viewTodo tick todo =
        let 
            pclass = if (tick >= .tick todo  && tick <= .tickEnd todo) then "is-current-todo"
                     else if (tick < .tick todo) then "is-future-todo" else "is-past-todo"
            aclass = case todo.action of
                         Pause -> " action-pause"
                         In -> " action-in"
                         Out -> " action-out"
                         Strike -> " action-strike"
        in 
                span [ class (pclass ++ aclass) ]
                    [ 
                           viewAction todo.action False
                    ]



---- PROGRAM ----

-- port saveTodos : List Todo -> Cmd msg
saveTodos : List Todo -> Cmd msg
saveTodos list =
    Cmd.none



subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



type alias Flags =
    { todos : List Int}


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

