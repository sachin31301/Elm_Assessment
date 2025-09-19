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


-- UPDATE

type Msg
    = InputChanged String
    | GenerateDistribution


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged newInput ->
            { model | input = newInput }

        GenerateDistribution ->
            case String.toInt model.input of
                Just n ->
                    if n >= 1 && n <= 100 then
                        let
                            dist = distributeFruits n
                        in
                        { model | distribution = dist }
                    else
                        { model | distribution = [] }

                Nothing ->
                    { model | distribution = [] }


-- VIEW

view : Model -> Html Msg
view model =
    div [ style "text-align" "center", style "margin" "20px"]
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


-- LOGIC

distributeFruits : Int -> List Int
distributeFruits total =
    let
        base = total // 7
        remainder = modBy 7 total

        days = List.range 1 7

    in
        List.map (\day -> distributionForDay day base remainder) days

distributionForDay day base remainder = 
                        if day <= remainder then
                          base +1 
                        else
                             base
    

-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
