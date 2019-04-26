module Main exposing (Board, Cell, Model, Msg(..), Point, Spice, Status(..), set, update, view)

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


type Area
    = One
    | Two
    | Four
    | Eight


type alias Spice =
    { id : String
    , name : String
    , color : String
    , oneCell : Bool
    , twoCell : Bool
    , fourCell : Bool
    , eightCell : Bool
    , selectedArea : Area
    }


type Status
    = Selected
    | Blank
    | SpiceSelected Spice


type alias Cell =
    { point : Point
    , status : Status
    }


type alias Board =
    List (List Cell)


type Modal
    = SpiceModal
    | DeleteModal Spice


type alias Model =
    { board : Board
    , spices : List Spice
    , modal : Maybe Modal
    , selectedSpice : Maybe Spice
    }


initialBoard : Board
initialBoard =
    List.range 1 4
        |> List.map
            (\y ->
                List.range 1 4
                    |> List.map
                        (\x -> Cell (Point x y) Blank)
            )


init : String -> ( Model, Cmd Msg )
init key =
    ( { board = initialBoard
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


set : Point -> Status -> Board -> Board
set point status =
    List.map
        (List.map
            (\cell ->
                if cell.point == point then
                    { cell | status = status }

                else
                    cell
            )
        )


get : Point -> Board -> Maybe Status
get point board =
    board
        |> List.foldr (++) []
        |> List.foldl
            (\cell status ->
                if cell.point == point then
                    Just cell.status

                else
                    status
            )
            Nothing


removeSelected : Board -> Board
removeSelected =
    List.map
        (List.map
            (\cell ->
                if cell.status == Selected then
                    { cell | status = Blank }

                else
                    cell
            )
        )


spiceInside : Point -> Point -> Board -> Bool
spiceInside startPoint endPoint =
    let
        between : Int -> Int -> Int -> Bool
        between a b value =
            if a >= value && value >= b then
                True

            else if b >= value && value >= a then
                True

            else
                False
    in
    List.foldr (++) []
        >> List.foldl
            (\cell inside ->
                inside
                    || (case cell.status of
                            SpiceSelected _ ->
                                between startPoint.x endPoint.x cell.point.x
                                    && between startPoint.y endPoint.y cell.point.y

                            _ ->
                                inside
                       )
            )
            False


pointsFromDiagnal : Point -> Point -> List Point
pointsFromDiagnal startPoint endPoint =
    let
        range : Int -> Int -> List Int
        range a b =
            if a > b then
                List.range b a

            else
                List.range a b

        xDiff =
            endPoint.x - startPoint.x

        yDiff =
            endPoint.y - startPoint.y

        points : List Point
        points =
            range 0 xDiff
                |> List.map
                    (\x ->
                        range 0 yDiff
                            |> List.map (\y -> Point (startPoint.x + x) (startPoint.y + y))
                    )
                |> List.foldr (++) []
    in
    points


firstSelected : Area -> Board -> Board
firstSelected area board =
    let
        selectablePoints =
            board
                |> List.foldr (++) []
                |> List.foldl
                    (\cell points ->
                        let
                            p =
                                cell.point

                            pointArea =
                                case area of
                                    One ->
                                        p

                                    Two ->
                                        { p | x = p.x + 1 }

                                    Four ->
                                        { p | x = p.x + 1, y = p.y + 1 }

                                    Eight ->
                                        { p | x = p.x + 1, y = p.y + 3 }
                        in
                        if List.isEmpty points && (not <| spiceInside cell.point pointArea board) then
                            pointsFromDiagnal cell.point pointArea

                        else
                            points
                    )
                    []
    in
    selectablePoints
        |> List.foldr (\p b -> b |> set p Selected) board


type Msg
    = FetchedValues (Result Http.Error (List (List String)))
    | CloseModal
    | SelectSpice Spice
    | AddSpice
    | ConfirmSpice
    | ChangeArea Area
    | RefreshBoard
    | OpenDeleteModal Spice
    | DeleteSpice Spice


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
                                , selectedArea = One
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
                        |> firstSelected One
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
                                                if cell.status == Selected then
                                                    { cell | status = SpiceSelected spice }

                                                else
                                                    cell
                                            )
                                        )
                          }
                        , Cmd.none
                        )
                    )
                |> Maybe.withDefault ( model, Cmd.none )

        ChangeArea area ->
            ( { model
                | board =
                    model.board
                        |> removeSelected
                        |> firstSelected area
                , selectedSpice = model.selectedSpice |> Maybe.map (\s -> { s | selectedArea = area })
              }
            , Cmd.none
            )

        RefreshBoard ->
            ( { model | board = initialBoard }, Cmd.none )

        OpenDeleteModal spice ->
            ( { model | modal = Just <| DeleteModal spice }, Cmd.none )

        DeleteSpice spice ->
            ( { model
                | board =
                    List.map
                        (List.map
                            (\cell ->
                                case cell.status of
                                    SpiceSelected s ->
                                        if s == spice then
                                            { cell | status = Blank }

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
              case selectedSpice of
                Just { name, oneCell, twoCell, fourCell, eightCell, selectedArea } ->
                    Html.div [ joinClasses [ "mb-3" ] ]
                        [ Html.div [ joinClasses [ "text-size-caption", "text-black55", "mb-2" ] ] [ Html.text "2. パズルの大きさを選んでください" ]
                        , Html.div [ joinClasses [ "flex" ] ]
                            [ if oneCell then
                                button [ Events.onClick <| ChangeArea One ] (selectedArea == One) "1個"

                              else
                                disabledButton
                            , if twoCell then
                                button [ Events.onClick <| ChangeArea Two ] (selectedArea == Two) "2個"

                              else
                                disabledButton
                            , if fourCell then
                                button [ Events.onClick <| ChangeArea Four ] (selectedArea == Four) "4個"

                              else
                                disabledButton
                            , if eightCell then
                                button [ Events.onClick <| ChangeArea Eight ] (selectedArea == Eight) "x8"

                              else
                                disabledButton
                            ]
                        ]

                Nothing ->
                    Html.text ""
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
                                                            Selected ->
                                                                [ Events.onClick ConfirmSpice, Attributes.style "background-color" "orange" ]

                                                            Blank ->
                                                                [ Attributes.style "background-color" "#fbfadfa3" ]

                                                            SpiceSelected spice ->
                                                                [ Events.onClick <| OpenDeleteModal spice, Attributes.style "background-color" spice.color ]
                                                       )
                                                )
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
                        Html.div []
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

                Nothing ->
                    Html.text ""

                Just SpiceModal ->
                    modalView <|
                        Html.div []
                            ([ Html.div [ joinClasses [ "text-black55", "text-size-caption" ] ] [ Html.text "スパイスを選択してください。" ]
                             ]
                                ++ (spices
                                        |> List.map
                                            (\s ->
                                                Html.div
                                                    [ joinClasses
                                                        [ "my-2"
                                                        , "text-size-body"
                                                        , "hover:text-black55"
                                                        , "cursor-pointer"
                                                        ]
                                                    , Events.onClick <| SelectSpice s
                                                    ]
                                                    [ Html.text s.name ]
                                            )
                                   )
                            )
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
