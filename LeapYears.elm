module LeapYears exposing (..)
import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (style, placeholder, value)
import Html.Events exposing (onInput, onClick)


main : Program () Model Msg
main =
    Browser.element {
        init = init,
        view = view,
        update = update,
        subscriptions = subscriptions
    }


type alias Model =
    { startYear : String
    , endYear : String
    , error : Maybe String
    , leapYears : List Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { startYear = "", endYear = "", error = Nothing, leapYears = [] }, Cmd.none )


type Msg
    = UpdateStartYear String
    | UpdateEndYear String
    | ShowLeapYears


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStartYear year ->
            ( { model | startYear = year, error = Nothing, leapYears = [] }, Cmd.none )

        UpdateEndYear year ->
            ( { model | endYear = year, error = Nothing, leapYears = [] }, Cmd.none )

        ShowLeapYears ->
            let
                ( maybeError, leapList ) =
                    validateAndFindLeapYears model.startYear model.endYear
            in
            ( { model
                | error = maybeError
                , leapYears = leapList
                , startYear = ""
                , endYear = ""
              }
            , Cmd.none
            )


validateAndFindLeapYears : String -> String -> ( Maybe String, List Int )
validateAndFindLeapYears startStr endStr =
    case ( String.toInt startStr, String.toInt endStr ) of
        ( Just start, Just end ) ->
            if start > end then
                ( Just "End Year must be greater than Start Year", [] )

            else
                let
                    yearsToCheck =
                        List.range (start + 1) (end - 1)

                    leapYears =
                        List.filter checkLeapYear yearsToCheck
                in
                ( Nothing, leapYears )

        _ ->
            ( Just "Please enter valid years", [] )


checkLeapYear : Int -> Bool
checkLeapYear year =
    (modBy 4 year == 0 && modBy 100 year /= 0)
        || (modBy 400 year == 0)


view : Model -> Html Msg
view model =
    div [ style "text-align" "center", style "margin" "40px" ]
        [ h1 [] [ text "Please enter start year and end year" ]
        , input [ placeholder "Start Year", value model.startYear, onInput UpdateStartYear ] []
        , input [ placeholder "End Year", value model.endYear, onInput UpdateEndYear ] []
        , button [ onClick ShowLeapYears ] [ text "Show Leap Years" ]
        , viewErrorOrLeapYears model
        ]


viewErrorOrLeapYears : Model -> Html Msg
viewErrorOrLeapYears model =
    case model.error of
        Just msg ->
            div [ style "color" "red", style "margin-top" "25px" ] [ text msg ]

        Nothing ->
            if List.isEmpty model.leapYears then
                text ""
            else
                div [ style "margin-top" "30px" ]
                    [ h1 [] [ text "List of Leap Years" ]
                    , ul [] (List.map viewYear model.leapYears)
                    ]


viewYear : Int -> Html msg
viewYear year =
    li [] [ text (String.fromInt year) ]
