module EmployeesApp exposing (..)
import Http
import Json.Decode as Decode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser

headerCellStyle =
    [ style "padding" "14px"
    , style "border" "1px solid #ddd"
    , style "background-color" "#f2f2f2"
    , style "font-weight" "bold"
    , style "text-align" "center"
    , style "font-size" "18px"
    ]

type alias Employee =
           {
            id: Int,
            name: String,
            age : Int,
            role : String,
            salary : Int
           }

type  RemoteData =  
                    Fetching |
                    Success ( List Employee) |
                    Error String
type alias Model =
    { employees  : RemoteData,
      seniors : List Employee
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( {employees = Fetching, seniors =[]}, fetchEmployees )

fetchEmployees = 
            Http.get 
            {
            url ="http://localhost:3000/employees"
            ,expect = Http.expectJson GotEmployees employeeListDecoder
            }

employeeListDecoder= 
            Decode.list employeeDecoder

employeeDecoder=
          Decode.map5 Employee
          ( Decode.field "id" Decode.int)
          ( Decode.field "name" Decode.string)
          ( Decode.field "age" Decode.int)
          ( Decode.field "role" Decode.string)
          ( Decode.field "salary" Decode.int)






type Msg
    = FetchEmployees
    | GotEmployees ( Result Http.Error ( List Employee ) ) |
    ShowSeniors 


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotEmployees result ->
            case result of
                 Ok employees ->
                   ({model | employees = Success employees}, Cmd.none)

                 Err error->
                    ({model | employees = Error (httpErrorToString error)}, Cmd.none)
        FetchEmployees ->
            ({model | employees= Fetching}, fetchEmployees)
        ShowSeniors ->
            ({model | seniors = getSeniors model.employees}, Cmd.none)


getSeniors employees =
                    case employees of
                        Success employeeslist->
                            employeeslist
                            |> List.filter (\e -> e.salary>25000)
                            |> List.map promoteToSenior
                            |> List.sortBy .name

                        _ ->
                            []
                    

promoteToSenior e=
                  { e | role = "Sr " ++ e.role}



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
        [ h1 [] [ text "Employee List" ]
        , button [ onClick ShowSeniors, style "margin-bottom" "20px" ] [ text "Show Senior Employees" ]
        , case model.employees of
            Fetching ->
                h2 [] [ text "Loading..." ]

            Error err ->
                h2 [] [ text err ]

            Success employees ->
                table
                    [ style "border-collapse" "collapse"
                    , style "margin" "auto"
                    , style "width" "80%"
                    , style "box-shadow" "0px 0px 10px rgba(0, 0, 0, 0.1)"
                    , style "border" "1px solid #ddd"
                    ]
                    [ thead []
                        [ tr []
                            [ th headerCellStyle [ text "ID" ]
                            , th headerCellStyle [ text "Name" ]
                            , th headerCellStyle [ text "Age" ]
                            , th headerCellStyle [ text "Role" ]
                            , th headerCellStyle [ text "Salary" ]
                            ]
                        ]
                    , tbody [] (List.map showEmployee employees)
                    ]

        -- Senior employees list (if any)
        , if not (List.isEmpty model.seniors) then
            div []
                [ h2 [ style "margin-top" "40px" ] [ text "Senior Employees" ]
                , table
                    [ style "border-collapse" "collapse"
                    , style "margin" "auto"
                    , style "width" "70%"
                    , style "border" "1px solid #ccc"
                    ]
                    [ thead []
                        [ tr []
                            [ th headerCellStyle [ text "ID" ]
                            , th headerCellStyle [ text "Name" ]
                            , th headerCellStyle [ text "Age" ]
                            , th headerCellStyle [ text "Role" ]
                            , th headerCellStyle [ text "Salary" ]
                            ]
                        ]
                    , tbody [] (List.map showEmployee model.seniors)
                    ]
                ]
          else
            text ""
        ]



                        

showEmployee employee=
        tr[][
            td [][text (String.fromInt employee.id)],
            td[][text employee.name],
            td [][text (String.fromInt employee.age)],
            td[][text employee.role],
            td [][text (String.fromInt employee.salary)]
            
        ]

main =
      Browser.element 
      {
        init=init,
        view=view,
        update=update,
        subscriptions=subscriptions
      }
