module GitUsers exposing (..)
import Browser exposing (..)
import Browser
import Html exposing (..)
import Http exposing (..)
import Json.Decode as Decode
import Html.Attributes exposing (src)
import Html.Attributes exposing (alt)
import Html.Attributes exposing (width)
import Html.Attributes exposing (height)
import Html.Attributes exposing (style)
import Html.Attributes exposing (title)





main =
      Browser.element 
      {
        init=init,
        view=view,
        update=update,
        subscriptions=subscriptions
      }

cellStyle : List (Html.Attribute msg)
cellStyle =
    [ style "padding" "12px"
    , style "border" "1px solid #ddd"
    , style "text-align" "center"
    , style "font-size" "16px"
    ]

headerCellStyle : List (Html.Attribute msg)
headerCellStyle =
    [ style "padding" "14px"
    , style "border" "1px solid #ddd"
    , style "background-color" "#f2f2f2"
    , style "font-weight" "bold"
    , style "text-align" "center"
    , style "font-size" "18px"
    ]


type alias User =
    {
        id : Int,
        login: String,
        avatar_url : String
    }

type  RemoteData =
                    Fetching |
                    Success ( List User) |
                    Error String
type alias Model =
    { users  : RemoteData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( {users = Fetching}, fetchUsers )

fetchUsers = 
            Http.get 
            {
            url ="https://api.github.com/users"
            , expect = Http.expectJson GotUsers userListDecoder
            }

userListDecoder= 
            Decode.list userDecoder

userDecoder=
          Decode.map3 User 
          ( Decode.field "id" Decode.int)
          ( Decode.field "login" Decode.string)
          ( Decode.field "avatar_url" Decode.string)






type Msg
    = FetchUsers
    | GotUsers ( Result Http.Error ( List User ) )


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotUsers result ->
            case result of
                 Ok users->
                   ({model | users = Success users}, Cmd.none)

                 Err error->
                    ({model | users = Error (httpErrorToString error)}, Cmd.none)
        FetchUsers ->
            ({model | users = Fetching}, fetchUsers)


          


httpErrorToString error =
    case error of
        Http.BadUrl msg -> "Bad URL: " ++ msg
        Http.Timeout -> "Request timed out"
        Http.NetworkError -> "Network error"
        Http.BadStatus status -> "Bad status: " ++ String.fromInt status
        Http.BadBody msg -> "Bad body: " ++ msg
    



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    div [ style "text-align" "center", style "margin" "40px" ]
        [ h1 [] [ text "GitHub User List" ]
        , case model.users of
            Fetching ->
                h2 [] [ text "Loading..." ]

            Error err ->
                h2 [] [ text err ]

            Success users ->
                table
                    [ style "border-collapse" "collapse"
                    , style "margin" "auto"
                    , style "width" "80%"
                    , style "box-shadow" "0px 0px 10px rgba(0, 0, 0, 0.1)"
                    , style "border" "1px solid #ddd"
                    ]
                    [ thead []
                        [ tr []
                            [ th (headerCellStyle) [ text "ID" ]
                            , th (headerCellStyle) [ text "Login ID" ]
                            , th (headerCellStyle) [ text "Avatar" ]
                            ]
                        ]
                    , tbody [] (List.map showUser users)
                    ]
        ]


showUser : User -> Html Msg
showUser user=
        tr[][
            td [][text (String.fromInt user.id)],
            td[][text user.login],
            td[][img [src user.avatar_url, alt "Image " , width 50, height 50][]]
        ]
