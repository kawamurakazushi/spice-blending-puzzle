module Main exposing (Modal(..), Model, Msg(..), init, main, subscriptions, update, view)

import Board
import Browser
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode
import List.Extra
import Spreadsheet


type Modal
    = SpiceModal
    | DeleteModal Board.Spice


type alias Model =
    { board : Board.Board
    , spices : List Board.Spice
    , modal : Maybe Modal
    , selectedSpice : Maybe Board.Spice
    }


init : String -> ( Model, Cmd Msg )
init key =
    ( { board = Board.initialBoard
      , spices = []
      , modal = Nothing
      , selectedSpice = Nothing
      }
    , Spreadsheet.getValues
        FetchedValues
        (Spreadsheet.Key key)
        (Spreadsheet.SpreadsheetId "1lnoOKBJ-bLpkRM9LNw0wVusbHzZdGgEX4Cok0lqbvRo")
        (Spreadsheet.SheetName "spices")
    )


type Msg
    = FetchedValues (Result Http.Error (List (List String)))
    | CloseModal
    | SelectSpice Board.Spice
    | AddSpice
    | ConfirmSpice
    | ChangeArea Board.Area
    | RefreshBoard
    | OpenDeleteModal Board.Spice
    | DeleteSpice Board.Spice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedValues (Ok values) ->
            let
                spices =
                    values
                        |> List.map
                            (\spice ->
                                let
                                    boolFromString b =
                                        if b == "1" then
                                            True

                                        else
                                            False
                                in
                                { id =
                                    spice
                                        |> List.Extra.getAt 0
                                        |> Maybe.withDefault ""
                                , name =
                                    spice
                                        |> List.Extra.getAt 1
                                        |> Maybe.withDefault ""
                                , color =
                                    spice
                                        |> List.Extra.getAt 2
                                        |> Maybe.withDefault ""
                                , oneCell =
                                    spice
                                        |> List.Extra.getAt 3
                                        |> Maybe.map boolFromString
                                        |> Maybe.withDefault False
                                , twoCell =
                                    spice
                                        |> List.Extra.getAt 4
                                        |> Maybe.map boolFromString
                                        |> Maybe.withDefault False
                                , fourCell =
                                    spice
                                        |> List.Extra.getAt 5
                                        |> Maybe.map boolFromString
                                        |> Maybe.withDefault False
                                , eightCell =
                                    spice
                                        |> List.Extra.getAt 6
                                        |> Maybe.map boolFromString
                                        |> Maybe.withDefault False
                                , selectedArea = Board.One
                                }
                            )
            in
            ( { model | spices = spices }, Cmd.none )

        FetchedValues (Err _) ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        SelectSpice spice ->
            ( { model
                | selectedSpice = Just spice
                , modal = Nothing
                , board =
                    model.board
                        |> Board.firstSelected Board.One
              }
            , Cmd.none
            )

        AddSpice ->
            ( { model | modal = Just SpiceModal }, Cmd.none )

        ConfirmSpice ->
            model.selectedSpice
                |> Maybe.map
                    (\spice ->
                        ( { model
                            | board =
                                model.board
                                    |> List.map
                                        (List.map
                                            (\cell ->
                                                if cell.status == Board.Selected then
                                                    { cell | status = Board.SpiceSelected spice }

                                                else
                                                    cell
                                            )
                                        )
                            , selectedSpice = Nothing
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ChangeArea area ->
            ( { model
                | board =
                    model.board
                        |> Board.removeSelected
                        |> Board.firstSelected area
                , selectedSpice = model.selectedSpice |> Maybe.map (\s -> { s | selectedArea = area })
              }
            , Cmd.none
            )

        RefreshBoard ->
            ( { model | board = Board.initialBoard }, Cmd.none )

        OpenDeleteModal spice ->
            ( { model | modal = Just <| DeleteModal spice }, Cmd.none )

        DeleteSpice spice ->
            ( { model
                | board =
                    List.map
                        (List.map
                            (\cell ->
                                case cell.status of
                                    Board.SpiceSelected s ->
                                        if s == spice then
                                            { cell | status = Board.Blank }

                                        else
                                            cell

                                    _ ->
                                        cell
                            )
                        )
                        model.board
                , modal = Nothing
              }
            , Cmd.none
            )


joinClasses : List String -> Html.Attribute msg
joinClasses =
    String.join " "
        >> Attributes.class


view : Model -> Html.Html Msg
view { board, spices, modal, selectedSpice } =
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
            [ Html.div [ joinClasses [ "flex-1", "overflow-auto" ] ]
                [ Html.div [ joinClasses [ "flex", "items-center", "mb-2" ] ]
                    [ Html.div [ joinClasses [ "flex-1", "text-size-h4", "font-secondary" ] ] [ Html.text "Spice Blending Puzzle" ]
                    , Html.button
                        [ Events.onClick RefreshBoard
                        , joinClasses [ "border", "border-black55", "rounded", "shadow-a", "text-black55", "text-size-small", "py-1", "px-2" ]
                        ]
                        [ Html.i [ Attributes.class "fa fa-redo-alt" ] [] ]
                    ]
                , Html.div [ joinClasses [ "text-size-caption", "text-black55", "my-2" ] ] [ Html.text "1. スパイスを選択してください" ]
                , Html.div [ joinClasses [ "flex", "items-center", "mb-3" ] ]
                    [ Html.button
                        [ Events.onClick AddSpice
                        , joinClasses [ "border", "border-black55", "rounded", "shadow-a", "text-black", "text-size-small", "px-3", "py-2" ]
                        ]
                        [ Html.text "スパイスを選択", Html.i [ joinClasses [ "fa", "fa-caret-down", "ml-2" ] ] [] ]
                    , case selectedSpice of
                        Just spice ->
                            Html.div [ joinClasses [ "flex-1", "flex", "items-center" ] ]
                                [ Html.div [ joinClasses [ "flex-1", "text-size-body", "font-bold", "ml-3" ] ] [ Html.text spice.name ]
                                ]

                        Nothing ->
                            Html.text ""
                    ]
                ]
            , let
                button attributes active text =
                    let
                        c =
                            if active then
                                [ "border-primary", "text-black90" ]

                            else
                                [ "text-black10" ]
                    in
                    Html.button (attributes ++ [ joinClasses ([ "flex-1", "border", "rounded", "mx-2", "shadow-a", "p-2" ] ++ c) ])
                        [ Html.div [ joinClasses [ "flex", "justify-center" ] ]
                            [ if active then
                                Html.i [ joinClasses [ "fa", "fa-check-circle", "text-primary", "text-size-small" ] ] []

                              else
                                Html.i [ joinClasses [ "fa", "fa-circle" ] ] []
                            ]
                        , Html.div [ joinClasses [ "text-size-caption", "mt-1" ] ] [ Html.text text ]
                        ]

                disabledButton =
                    Html.div
                        [ joinClasses
                            [ "flex-1"
                            , "border"
                            , "rounded"
                            , "mx-2"
                            , "shadow-a"
                            , "p-2"
                            , "bg-black10"
                            , "text-black55"
                            , "flex"
                            , "items-center"
                            , "justify-center"
                            ]
                        ]
                        [ Html.i [ joinClasses [ "fa", "fa-times", "text-size-small" ] ] [] ]
              in
              Html.div [ joinClasses [ "mb-3" ] ]
                [ Html.div [ joinClasses [ "text-size-caption", "text-black55", "mb-2" ] ] [ Html.text "2. パズルの大きさを選んでください" ]
                , Html.div [ joinClasses [ "flex" ] ] <|
                    case selectedSpice of
                        Just { name, oneCell, twoCell, fourCell, eightCell, selectedArea } ->
                            [ if oneCell then
                                button [ Events.onClick <| ChangeArea Board.One ] (selectedArea == Board.One) "1個"

                              else
                                disabledButton
                            , if twoCell then
                                button [ Events.onClick <| ChangeArea Board.Two ] (selectedArea == Board.Two) "2個"

                              else
                                disabledButton
                            , if fourCell then
                                button [ Events.onClick <| ChangeArea Board.Four ] (selectedArea == Board.Four) "4個"

                              else
                                disabledButton
                            , if eightCell then
                                button [ Events.onClick <| ChangeArea Board.Eight ] (selectedArea == Board.Eight) "8個"

                              else
                                disabledButton
                            ]

                        Nothing ->
                            List.repeat 4 disabledButton
                ]
            , Html.div [ joinClasses [ "text-size-caption", "text-black55", "mb-2" ] ] [ Html.text "3. 場所を決定してください" ]
            , Html.div [ joinClasses [ "flex", "flex-col", "items-center" ] ]
                (board
                    |> List.map
                        (\line ->
                            Html.div [ joinClasses [ "flex", "w-full" ] ] <|
                                (line
                                    |> List.map
                                        (\{ status, point } ->
                                            Html.div
                                                ([ joinClasses [ "box", "text-size-caption", "flex", "justify-center", "items-center", "border-r", "border-b", "border-white" ]
                                                 ]
                                                    ++ (case status of
                                                            Board.Selected ->
                                                                [ Events.onClick ConfirmSpice, Attributes.style "background-color" "orange" ]

                                                            Board.Blank ->
                                                                [ Attributes.style "background-color" "#fbfadfa3" ]

                                                            Board.SpiceSelected spice ->
                                                                [ Events.onClick <| OpenDeleteModal spice, Attributes.style "background-color" spice.color ]
                                                       )
                                                )
                                                [ case status of
                                                    Board.SpiceSelected { name } ->
                                                        Html.text name

                                                    _ ->
                                                        Html.text ""
                                                ]
                                        )
                                )
                        )
                )
            , let
                modalView a =
                    Html.div [ joinClasses [ "fixed", "pin", "bg-white55", "flex", "justify-center", "items-center" ] ]
                        [ Html.div [ joinClasses [ "bg-white", "max-w-content", "w-full", "shadow-a", "p-4", "rounded" ] ]
                            [ Html.div [ joinClasses [] ]
                                [ a
                                ]
                            ]
                        ]
              in
              case modal of
                Just (DeleteModal spice) ->
                    modalView <|
                        Html.div [ joinClasses [] ]
                            [ Html.div [ joinClasses [ "text-size-body", "mb-3" ] ] [ Html.text "選択したスパイスを解除しますか？" ]
                            , Html.div [ joinClasses [ "text-size-caption", "mb-1", "text-black55" ] ] [ Html.text "選択中のスパイス:" ]
                            , Html.div [ joinClasses [ "text-size-caption", "mb-3", "text-black90", "font-bold" ] ] [ Html.text spice.name ]
                            , Html.div [ joinClasses [ "flex", "w-full" ] ]
                                [ Html.button
                                    [ Events.onClick CloseModal
                                    , joinClasses
                                        [ "flex-1", "rounded", "text-size-small", "shadow-a", "p-2", "text-black55", "mr-2" ]
                                    ]
                                    [ Html.text "キャンセル" ]
                                , Html.button
                                    [ Events.onClick <| DeleteSpice spice
                                    , joinClasses
                                        [ "flex-1", "rounded", "text-size-small", "shadow-a", "p-2", "bg-error", "text-white", "ml-2" ]
                                    ]
                                    [ Html.text "削除" ]
                                ]
                            ]

                Just SpiceModal ->
                    modalView <|
                        Html.div []
                            [ Html.div [ joinClasses [ "text-size-body", "mb-3" ] ] [ Html.text "スパイスを選択してください。" ]
                            , Html.table [ joinClasses [ "w-full", "text-size-caption" ] ] <|
                                [ Html.tr [ joinClasses [ "text-black55", "border-b" ] ]
                                    [ Html.td [ joinClasses [ "p-2" ] ] [ Html.text "" ]
                                    , Html.td [ joinClasses [ "p-2" ] ] [ Html.text "スパイス" ]
                                    , Html.td [ joinClasses [ "p-2" ] ] [ Html.text "1個" ]
                                    , Html.td [ joinClasses [ "p-2" ] ] [ Html.text "2個" ]
                                    , Html.td [ joinClasses [ "p-2" ] ] [ Html.text "4個" ]
                                    , Html.td [ joinClasses [ "p-2" ] ] [ Html.text "8個" ]
                                    ]
                                ]
                                    ++ (spices
                                            |> List.map
                                                (\s ->
                                                    let
                                                        checkView active =
                                                            Html.td
                                                                [ joinClasses <|
                                                                    [ "p-2" ]
                                                                        ++ (if Board.include s board then
                                                                                [ "text-black10" ]

                                                                            else
                                                                                [ "text-primary" ]
                                                                           )
                                                                ]
                                                                [ if active then
                                                                    Html.i [ joinClasses [ "fa", "fa-check-square" ] ] []

                                                                  else
                                                                    Html.text ""
                                                                ]
                                                    in
                                                    Html.tr
                                                        ([ joinClasses
                                                            [ "my-2"
                                                            , "hover:text-black55"
                                                            , "cursor-pointer"
                                                            , "border-b"
                                                            ]
                                                         ]
                                                            ++ (if Board.include s board then
                                                                    []

                                                                else
                                                                    [ Events.onClick <| SelectSpice s ]
                                                               )
                                                        )
                                                        [ Html.td []
                                                            [ if Board.include s board then
                                                                Html.i [ joinClasses [ "fa", "fa-star-of-life", "text-black10" ] ] []

                                                              else
                                                                Html.text ""
                                                            ]
                                                        , Html.td
                                                            [ joinClasses <|
                                                                [ "font-bold", "p-2" ]
                                                                    ++ (if Board.include s board then
                                                                            [ "text-black10" ]

                                                                        else
                                                                            []
                                                                       )
                                                            ]
                                                            [ Html.text s.name ]
                                                        , checkView s.oneCell
                                                        , checkView s.twoCell
                                                        , checkView s.fourCell
                                                        , checkView s.eightCell
                                                        ]
                                                )
                                       )
                            ]

                Nothing ->
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
