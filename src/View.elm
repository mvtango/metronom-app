module View exposing (..)

import Html exposing (Html, a, button, text, div, h1, img)
import Html.Attributes exposing (src, class)

main : Html
main =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [] [
                button [ class "button is-small is-danger" ] [text "danger"]
        ]
        ]


