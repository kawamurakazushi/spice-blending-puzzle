module Main exposing (Msg, init, update)

import Browser
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode
import List.Extra
import Spreadsheet


type alias Point =
    { x : Int
    , y : Int
    }


type alias Spice =
    { id : String, name : String, color : String }


type Status
    = Selected
    | Blank
    | SpiceSelected Spice


type alias Cell =
    { point : Point
    , status : Status
    }


type alias Model =
    { startAt : Maybe Point
    , dragging : Bool
    , board : List (List Cell)
    , spices : List (List String)
    , spiceModal : Bool
    }


init : String -> ( Model, Cmd Msg )
init key =
    ( { startAt = Nothing
      , dragging = False
      , board =
            List.range 1 4
                |> List.map
                    (\y ->
                        List.range 1 4
                            |> List.map
                                (\x -> Cell (Point x y) Blank)
                    )
      , spices = []
      , spiceModal = False
      }
    , Spreadsheet.getValues
        FetchedValues
        (Spreadsheet.Key key)
        (Spreadsheet.SpreadsheetId "1lnoOKBJ-bLpkRM9LNw0wVusbHzZdGgEX4Cok0lqbvRo")
        (Spreadsheet.SheetName "spices")
    )


type Msg
    = OnMouseDown Point
    | OnMouseUp Point
    | OnMouseEnter Point
    | FetchedValues (Result Http.Error (List (List String)))
    | CloseModal
    | SelectSpice Spice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        between : Int -> Int -> Int -> Bool
        between a b value =
            if a >= b then
                value <= a && value >= b

            else
                value <= b && value >= a
    in
    case msg of
        OnMouseDown point ->
            ( { model
                | board =
                    model.board
                        |> List.map
                            (List.map
                                (\cell ->
                                    case cell.status of
                                        SpiceSelected _ ->
                                            cell

                                        _ ->
                                            if cell.point == point then
                                                { cell | status = Selected }

                                            else
                                                { cell | status = Blank }
                                )
                            )
                , startAt = Just point
                , dragging = True
              }
            , Cmd.none
            )

        OnMouseUp point ->
            ( { model | dragging = False, spiceModal = True }, Cmd.none )

        OnMouseEnter point ->
            let
                active : Point -> Bool
                active p =
                    case ( model.startAt, point ) of
                        ( Just startAt, endAt ) ->
                            between startAt.x endAt.x p.x && between startAt.y endAt.y p.y

                        _ ->
                            False
            in
            ( { model
                | board =
                    if model.dragging then
                        model.board
                            |> List.map
                                (List.map
                                    (\cell ->
                                        case cell.status of
                                            SpiceSelected _ ->
                                                cell

                                            _ ->
                                                if active cell.point then
                                                    { cell | status = Selected }

                                                else
                                                    { cell | status = Blank }
                                    )
                                )

                    else
                        model.board
              }
            , Cmd.none
            )

        FetchedValues (Ok values) ->
            ( { model | spices = values }, Cmd.none )

        FetchedValues (Err _) ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | spiceModal = False }, Cmd.none )

        SelectSpice spice ->
            ( { model
                | board =
                    model.board
                        |> List.map
                            (List.map
                                (\cell ->
                                    if cell.status == Selected then
                                        { cell | status = SpiceSelected spice }

                                    else
                                        cell
                                )
                            )
                , spiceModal = False
              }
            , Cmd.none
            )


joinClasses : List String -> Html.Attribute msg
joinClasses =
    String.join " "
        >> Attributes.class


view : Model -> Html.Html Msg
view { board, spices, spiceModal } =
    let
        spiceModalView =
            Html.div
                [ joinClasses
                    [ "fixed"
                    , "pin"
                    , "bg-white55"
                    , "flex"
                    , "justify-center"
                    , "items-center"
                    ]
                ]
                [ Html.div
                    [ joinClasses
                        [ "bg-white"
                        , "max-w-content"
                        , "w-full"
                        , "shadow-a"
                        , "p-3"
                        , "rounded"
                        ]
                    ]
                  <|
                    [ Html.div
                        [ joinClasses
                            [ "flex"
                            , "flex-row-reverse"
                            , "justify-between"
                            , "items-center"
                            ]
                        ]
                        [ Html.div
                            [ joinClasses
                                [ "cursor-pointer"
                                , "text-black55"
                                , "text-size-h5"
                                , "hover:text-black10"
                                , "p-2"
                                ]
                            , Events.onClick CloseModal
                            ]
                            [ Html.text "×" ]
                        , Html.div
                            [ joinClasses
                                [ "text-black55"
                                , "text-size-caption"
                                ]
                            ]
                            [ Html.text "スパイスを選択してください" ]
                        ]
                    ]
                        ++ (spices
                                |> List.map
                                    (\s ->
                                        let
                                            id =
                                                s
                                                    |> List.Extra.getAt 0
                                                    |> Maybe.withDefault ""

                                            name =
                                                s
                                                    |> List.Extra.getAt 1
                                                    |> Maybe.withDefault ""

                                            color =
                                                s
                                                    |> List.Extra.getAt 2
                                                    |> Maybe.withDefault ""
                                        in
                                        Html.div
                                            [ joinClasses
                                                [ "my-2"
                                                , "text-size-body"
                                                , "hover:text-black55"
                                                , "cursor-pointer"
                                                ]
                                            , Events.onClick <| SelectSpice <| Spice id name color
                                            ]
                                            [ Html.text name ]
                                    )
                           )
                ]
    in
    Html.div
        [ joinClasses [ "flex", "justify-center" ] ]
        [ Html.div
            [ joinClasses
                [ "max-w-content"
                , "w-full"
                , "flex"
                , "flex-col"
                , "justify-center"
                , "p-3"
                ]
            ]
            [ Html.div [ joinClasses [ "text-size-h5" ] ] [ Html.text "スパイスブレンディングメソッド" ]
            , Html.div [ joinClasses [ "flex", "flex-col", "items-center" ] ]
                (board
                    |> List.map
                        (\line ->
                            Html.div [ Attributes.style "display" "flex" ] <|
                                (line
                                    |> List.map
                                        (\{ status, point } ->
                                            Html.div
                                                [ joinClasses [ "w-box", "h-box", "text-size-caption", "flex", "justify-center", "items-center" ]
                                                , case status of
                                                    Selected ->
                                                        Attributes.style "border" "1px solid orange"

                                                    Blank ->
                                                        Attributes.style "border" "1px solid #444"

                                                    SpiceSelected spice ->
                                                        Attributes.style "background-color" spice.color
                                                , Events.onMouseDown <| OnMouseDown point
                                                , Events.onMouseEnter <| OnMouseEnter point
                                                , Events.onMouseUp <| OnMouseUp point
                                                ]
                                                -- , Events.on "touchmove" <| Json.Decode.succeed (OnMouseEnter point)
                                                -- , Events.on "touchstart" <| Json.Decode.succeed (OnMouseDown point)
                                                -- , Events.on "touchend" <| Json.Decode.succeed (OnMouseUp point)
                                                [ case status of
                                                    SpiceSelected { name } ->
                                                        Html.text name

                                                    _ ->
                                                        Html.text ""
                                                ]
                                        )
                                )
                        )
                )
            , if spiceModal then
                spiceModalView

              else
                Html.text ""
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
