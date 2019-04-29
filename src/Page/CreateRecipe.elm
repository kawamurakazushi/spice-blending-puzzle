module Page.CreateRecipe exposing (Modal(..), Model, Msg(..), init, update, view)

import Board
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
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
init apiKey =
    ( { board = Board.initialBoard
      , spices = []
      , modal = Nothing
      , selectedSpice = Nothing
      }
    , Spreadsheet.getValues
        FetchedValues
        (Spreadsheet.Key apiKey)
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
                        |> Board.selected Board.One
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
                            | board = model.board |> Board.confirmSpice spice
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
                        |> Board.selected area
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
                | board = Board.remove (Board.SpiceSelected spice) model.board
                , modal = Nothing
              }
            , Cmd.none
            )


joinClasses : List String -> Html.Attribute msg
joinClasses =
    String.join " "
        >> Attributes.class


view : Model -> Html.Html Msg
view { selectedSpice, board, modal, spices } =
    let
        questionsView : Html.Html Msg
        questionsView =
            Html.div []
                [ Html.div [ joinClasses [ "text-size-caption", "text-black55", "my-2" ] ] [ Html.text "1. スパイスを選択してください" ]
                , Html.div [ joinClasses [ "flex", "items-center", "mb-3" ] ]
                    [ Html.button
                        [ Events.onClick AddSpice
                        , joinClasses [ "border", "border-black50", "rounded", "shadow-a", "text-size-small", "px-3", "py-2" ]
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
                                ]
                            ]
                            [ Html.div [ joinClasses [ "flex", "justify-center" ] ]
                                [ Html.div [ joinClasses [ "text-size-small" ] ] [ Html.text "\u{3000}" ]
                                ]
                            , Html.div [ joinClasses [ "text-size-caption", "mt-1" ] ] [ Html.text "\u{3000}" ]
                            ]
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
                ]

        puzzleView : Html.Html Msg
        puzzleView =
            Html.div [ Attributes.style "display" "grid", Attributes.style "grid-template-rows" "92px 92px 92px 92px", Attributes.style "grid-template-columns" "1fr 1fr 1fr 1fr " ] <|
                (board
                    |> List.map
                        (\{ row, col, status } ->
                            Html.div
                                ([ Attributes.style "grid-row" ((row |> Tuple.first |> String.fromInt) ++ "/" ++ (row |> Tuple.second |> String.fromInt))
                                 , Attributes.style "grid-column" ((col |> Tuple.first |> String.fromInt) ++ "/" ++ (col |> Tuple.second |> String.fromInt))
                                 , joinClasses <|
                                    [ "border", "border-white", "text-size-caption", "font-bold", "flex", "justify-center", "items-center" ]
                                        ++ (case status of
                                                Board.Selected ->
                                                    [ "z-10", "shadow-b" ]

                                                Board.SpiceSelected _ ->
                                                    [ "" ]

                                                Board.Blank ->
                                                    [ "bg-black10" ]
                                           )
                                 ]
                                    ++ (case status of
                                            Board.Selected ->
                                                [ Attributes.style "outline" "3px solid oldlace"
                                                , Events.onClick <| ConfirmSpice
                                                ]

                                            Board.SpiceSelected spice ->
                                                [ Attributes.style "background-color" (spice.color ++ "70"), Events.onClick <| OpenDeleteModal spice ]

                                            _ ->
                                                []
                                       )
                                )
                                (case status of
                                    Board.Selected ->
                                        [ Html.div [ joinClasses [ "rounded-full", "p-3", "shadow-a" ], Attributes.style "background-color" "oldlace" ] [ Html.text "決定" ] ]

                                    Board.SpiceSelected spice ->
                                        [ Html.text spice.name ]

                                    _ ->
                                        [ Html.text "" ]
                                )
                        )
                )
    in
    Html.div []
        [ if Board.completed board then
            Html.div [ joinClasses [ "border-b", "border-black10", "mb-2" ] ]
                [ Html.div [ joinClasses [ "my-2", "text-size-small", "font-bold" ] ] [ Html.text "オリジナルスパイスブレンド" ] ]

          else
            questionsView
        , puzzleView
        , if Board.completed board then
            Html.div []
                [ Html.div [ joinClasses [ "border-b", "border-black10", "mb-2" ] ]
                    [ Html.div [ joinClasses [ "my-2", "text-size-small", "font-bold" ] ] [ Html.text "レシピ (4人分)" ] ]
                , Html.div [ Attributes.style "display" "grid", Attributes.style "grid-template-columns" "1fr 100px" ]
                    (board
                        |> List.map
                            (\cell ->
                                case cell.status of
                                    Board.SpiceSelected spice ->
                                        [ Html.div [ joinClasses [ "my-1", "text-size-small" ] ] [ Html.text spice.name ]
                                        , Html.div [ joinClasses [ "my-1", "text-size-small" ] ] [ Html.text <| "小さじ " ++ (cell |> Board.cells |> List.length |> toFloat |> (*) 0.5 |> String.fromFloat) ]
                                        ]

                                    _ ->
                                        [ Html.div [] [] ]
                            )
                        |> List.foldl (++) []
                    )
                ]

          else
            Html.div [] []
        , let
            modalView body =
                Html.div [ joinClasses [ "fixed", "pin", "bg-white55", "flex", "justify-center", "items-center", "z-20" ] ]
                    [ Html.div [ joinClasses [ "bg-white", "max-w-content", "w-full", "shadow-a", "p-4", "rounded" ] ]
                        [ Html.div [ joinClasses [] ]
                            [ body
                            ]
                        ]
                    ]
          in
          case modal of
            Just (DeleteModal spice) ->
                modalView <|
                    Html.div [ joinClasses [] ]
                        [ Html.div [ joinClasses [ "flex" ] ]
                            [ Html.div [ joinClasses [ "flex-1", "text-size-body", "mb-3" ] ] [ Html.text "選択したスパイスを解除しますか？" ]
                            ]
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
                        [ Html.div [ joinClasses [ "flex", "mb-3", "items-center" ] ]
                            [ Html.div [ joinClasses [ "flex-1", "text-size-body" ] ] [ Html.text "スパイスを選択してください。" ]
                            , Html.i [ Events.onClick CloseModal, joinClasses [ "fa", "fa-times", "text-size-h5", "text-black55", "p-2" ] ] []
                            ]
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
