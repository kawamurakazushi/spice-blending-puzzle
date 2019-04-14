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


type Status
    = Selected
    | NotSelected


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
                                (\x -> Cell (Point x y) NotSelected)
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
                                    if cell.point == point then
                                        { cell | status = Selected }

                                    else
                                        { cell | status = NotSelected }
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
                                        if active cell.point then
                                            { cell | status = Selected }

                                        else
                                            { cell | status = NotSelected }
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


view : Model -> Html.Html Msg
view { board, spices, spiceModal } =
    let
        spiceModalView =
            Html.div
                [ Attributes.style "position" "fixed"
                , Attributes.style "top" "0"
                , Attributes.style "bottom" "0"
                , Attributes.style "right" "0"
                , Attributes.style "left" "0"
                , Attributes.style "background-color" "rgba(255, 255, 255, 0.7)"
                , Attributes.style "display" "flex"
                , Attributes.style "justify-content" "center"
                , Attributes.style "align-items" "center"
                , Events.onClick CloseModal
                ]
                [ Html.div
                    [ Attributes.style "background-color" "#fff"
                    , Attributes.style "max-width" "400px"
                    , Attributes.style "padding" "24px"
                    , Attributes.style "border-radius" "8px"
                    , Attributes.style "box-shadow" "0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24)"
                    ]
                    (spices
                        |> List.map
                            (List.Extra.getAt 0
                                >> Maybe.withDefault ""
                                >> Html.text
                                >> List.singleton
                                >> Html.div []
                            )
                    )
                ]
    in
    Html.div
        [ Attributes.style "max-width" "400px"
        , Attributes.style "width" "100%"
        , Attributes.style "display" "flex"
        , Attributes.style "flex-direction" "column"
        , Attributes.style "align-items" "center"
        , Attributes.style "margin" "0 auto"
        ]
        [ Html.h1 [] [ Html.text "スパイスブレンディングメソッド" ]
        , Html.div []
            (board
                |> List.map
                    (\line ->
                        Html.div [ Attributes.style "display" "flex" ] <|
                            (line
                                |> List.map
                                    (\{ status, point } ->
                                        Html.div
                                            [ Attributes.style "width" "50px"
                                            , Attributes.style "height" "50px"
                                            , case status of
                                                Selected ->
                                                    Attributes.style "border" "1px solid orange"

                                                NotSelected ->
                                                    Attributes.style "border" "1px solid #444"
                                            , Events.onMouseDown <| OnMouseDown point
                                            , Events.onMouseEnter <| OnMouseEnter point
                                            , Events.onMouseUp <| OnMouseUp point
                                            ]
                                            -- , Events.on "touchmove" <| Json.Decode.succeed (OnMouseEnter point)
                                            -- , Events.on "touchstart" <| Json.Decode.succeed (OnMouseDown point)
                                            -- , Events.on "touchend" <| Json.Decode.succeed (OnMouseUp point)
                                            []
                                    )
                            )
                    )
            )
        , if spiceModal then
            spiceModalView

          else
            Html.text ""
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
