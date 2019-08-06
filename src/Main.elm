port module Main exposing (..)

import Browser
import Platform.Cmd
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
    | SetTick Int
    | ResetTodos
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
    ( Model [ Todo Pause 0 1 ]
            Stopped 
            -1 
            2 
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

findSound: Action -> Cmd Msg
findSound action =  case action of 
        In -> playSample "in"
        Out -> playSample "out"
        Strike -> playSample "strike" -- playSample "strike"
        Pause -> Cmd.none

findCurrentSound: Model -> Cmd Msg
findCurrentSound model =
    let
        this = List.filter (\t -> t.tick == model.tick) model.todos
               |> List.head
    in
        case this of
            Nothing -> Cmd.none
            Just a -> findSound a.action

todosLength: List Todo -> Int
todosLength todos =
    let
        last = List.head (List.reverse todos)
    in
        case last of
            Nothing -> 2
            Just a -> a.tickEnd

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
             , Platform.Cmd.batch [saveTodos newTodos, findSound action]
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
                       Looping   -> ({ model | tick = remainderBy model.loop (model.tick + 1) }, findCurrentSound model)
                     )

        SetTick pos -> ({ model | tick = pos - 1, mode = Looping }, Cmd.none)

        ResetTodos  -> (Model [ Todo Pause 0 1 ]
                            Stopped 
                            -1 
                            2 
                            False, Cmd.none)

        UpdateMode newmode -> case newmode of
                  Looping   -> ({ model | mode = newmode, loop = todosLength model.todos, tick = 0 }, Cmd.none)
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
    let 
        resetButton = 
                button [ onClick (ResetTodos), class "button is-primary", disabled (model.mode /= Stopped) ] [
                               span [ class "icon" ] 
                                  [
                                    Html.i [ class "fas fa-undo" ] []
                                  ]
                            ]
    in 
    div [ ]
        [
        case model.mode of
          Stopped ->
                     span [] [
                          resetButton
                        , viewButton Looping False
                        , viewButton Stopped True
                        , viewButton Recording False
                    ]
          Looping ->
                     span [] [
                          resetButton
                        , viewButton Looping True
                        , viewButton Stopped False
                        , viewButton Recording True
                    ]
          Recording ->
                     span [] [
                          resetButton
                        , viewButton Looping True
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
                 , br [] []
                 , div [    class ""
                       , style "opacity" (if model.mode == Recording then "1" else "0") 
                     ] 
                  [
                         viewAction Strike Nothing
                       , (if model.inside then viewAction Out Nothing else text "")
                       , (if not model.inside then viewAction In Nothing else text "")

                  ]
                ]
                , div [ class "level-item" ]
                [
                 div [ class "box", style "line-height" "2.1em", style "text-align" "left" ] 
                     (List.map (viewTodo model.tick) (if model.mode == Recording 
                                                      then addPause model.todos model.tick 
                                                      else model.todos))

                ]
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


viewAction: Action -> Maybe Int -> Html Msg
viewAction action pos =
    let
        clickaction = onClick (case pos of 
                               Nothing -> AddTodo action 
                               Just i -> SetTick i)
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
    div [ class "section" ] [
          viewHeader model 

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
            ping   = if (tick == todo.tick) then 
                        case todo.action of
                            Strike -> playSample "strike"
                            In     -> playSample "in"
                            Out    -> playSample "out"
                            Pause  -> Cmd.none
                     else Cmd.none
        in 
                span [ class (pclass ++ aclass) ]
                    [ 
                           viewAction todo.action (Just todo.tick)
                    ]



---- PROGRAM ----

-- port saveTodos : List Todo -> Cmd msg
saveTodos : List Todo -> Cmd msg
saveTodos list =
    Cmd.none

port playSample: String -> Cmd msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 20 Tick



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

