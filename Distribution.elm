module Distribution exposing (..)

import Browser
import Html exposing (Html, div, input, text, button, ul, li)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import String

type alias Model =
    { input : String
    , distribution : List Int
    }


init : Model
init =
    { input = ""
    , distribution = []
    }


type Msg
    = InputChanged String
    | GenerateDistribution


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newInput ->
            { model | input = newInput }

        GenerateDistribution ->
            let
                dist = validateAndDistribute model.input
            in
            { model | distribution = dist }



validateAndDistribute : String -> List Int
validateAndDistribute inputStr =
    case String.toInt inputStr of
        Just n ->
            if n >= 1 && n <= 100 then
                distributeFruits n
            else
                []
        Nothing ->
            []




distributeFruits : Int -> List Int
distributeFruits total =
    let
        base = total // 7
        remainder = modBy 7 total
        days = List.range 1 7
    in
        List.map (\day -> distributionForDay day base remainder) days


distributionForDay : Int -> Int -> Int -> Int
distributionForDay day base remainder =
    if day <= remainder then
        base + 1
    else
        base




view : Model -> Html Msg
view model =
    div [ style "text-align" "center", style "margin" "20px" ]
        [ input
            [ placeholder "Enter number of fruits"
            , value model.input
            , onInput InputChanged
            ]
            []
        , button [ onClick GenerateDistribution ] [ text "Distribute" ]
        , div [] [ text "Distribution over 7 days:" ]
        , ul [] (List.map (\n -> li [] [ text (String.fromInt n) ]) model.distribution)
        ]




main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
