module Page.CreateRecipe exposing (Modal(..), Model, Msg(..), init, update, view)

import Api
import Board
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import List.Extra
import Spice
import Url
import Url.Builder
import View.Board
import View.Recipe as Recipe


type Modal
    = SpiceModal
    | DeleteModal Spice.Spice


type alias Model =
    { board : Board.Board
    , spices : List Spice.Spice
    , modal : Maybe Modal
    , selectedSpice : Maybe { spice : Spice.Spice, area : Board.Area }
    , comment : String
    , sending : Bool
    , key : Nav.Key
    }


init : Nav.Key -> String -> ( Model, Cmd Msg )
init key apiKey =
    ( { board = Board.initialBoard
      , spices = []
      , modal = Nothing
      , selectedSpice = Nothing
      , comment = ""
      , sending = False
      , key = key
      }
    , Http.get
        { url = Api.url ++ Url.Builder.toQuery [ Url.Builder.string "resource" "spices" ]
        , expect = Http.expectJson FetchedValues (Decode.list Spice.decoder)
        }
    )


type Msg
    = FetchedValues (Result Http.Error (List Spice.Spice))
    | CloseModal
    | SelectSpice Spice.Spice
    | AddSpice
    | ConfirmSpice
    | ChangeArea Board.Area
    | RefreshBoard
    | OpenDeleteModal Spice.Spice
    | DeleteSpice Spice.Spice
    | ShareRecipe
    | SharedRecipe (Result Http.Error String)
    | InputComment String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedValues (Ok values) ->
            ( { model
                | spices = values
              }
            , Cmd.none
            )

        FetchedValues (Err e) ->
            ( model, Cmd.none )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        SelectSpice spice ->
            ( { model
                | selectedSpice = Just { spice = spice, area = Board.One }
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
                    (\{ spice } ->
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
                , selectedSpice =
                    model.selectedSpice
                        |> Maybe.map (\s -> { s | area = area })
              }
            , Cmd.none
            )

        RefreshBoard ->
            ( { model | board = Board.initialBoard }, Cmd.none )

        OpenDeleteModal spice ->
            ( { model | modal = Just <| DeleteModal spice }, Cmd.none )

        DeleteSpice spice ->
            ( { model
                | board = Board.remove (Board.SpiceSelected spice Board.One) model.board
                , modal = Nothing
              }
            , Cmd.none
            )

        ShareRecipe ->
            let
                decoder : Decode.Decoder String
                decoder =
                    Decode.map identity
                        (Decode.field "message" Decode.string)

                puzzle : Encode.Value
                puzzle =
                    model.board
                        |> Encode.list
                            (\cell ->
                                let
                                    ( colStart, colEnd ) =
                                        cell.col

                                    ( rowStart, rowEnd ) =
                                        cell.row
                                in
                                case cell.status of
                                    Board.SpiceSelected spice _ ->
                                        Encode.object
                                            [ ( "colStart", Encode.int colStart )
                                            , ( "colEnd", Encode.int colEnd )
                                            , ( "rowStart", Encode.int rowStart )
                                            , ( "rowEnd", Encode.int rowEnd )
                                            , ( "id", Encode.int spice.id )
                                            ]

                                    _ ->
                                        Encode.object
                                            [ ( "colStart", Encode.int colStart )
                                            , ( "colEnd", Encode.int colEnd )
                                            , ( "rowStart", Encode.int rowStart )
                                            , ( "rowEnd", Encode.int rowEnd )
                                            ]
                            )

                query =
                    Url.Builder.toQuery [ Url.Builder.string "puzzle" (Encode.encode 0 puzzle), Url.Builder.string "comment" model.comment ]
            in
            ( { model | sending = True }
            , Http.post
                { url = Api.url ++ query
                , body = Http.emptyBody
                , expect = Http.expectJson SharedRecipe decoder
                }
            )

        SharedRecipe (Ok _) ->
            ( { model | sending = False }, Nav.pushUrl model.key "/recipes" )

        SharedRecipe (Err _) ->
            ( model, Cmd.none )

        InputComment value ->
            ( { model | comment = value }, Cmd.none )


joinClasses : List String -> Html.Attribute msg
joinClasses =
    String.join " "
        >> Attributes.class


view : Model -> Html.Html Msg
view { selectedSpice, board, modal, spices, comment, sending } =
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
                        Just { spice } ->
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
                            Just { spice, area } ->
                                [ if spice.canOne then
                                    button [ Events.onClick <| ChangeArea Board.One ] (area == Board.One) "1個"

                                  else
                                    disabledButton
                                , if spice.canTwo then
                                    button [ Events.onClick <| ChangeArea Board.Two ] (area == Board.Two) "2個"

                                  else
                                    disabledButton
                                , if spice.canFour then
                                    button [ Events.onClick <| ChangeArea Board.Four ] (area == Board.Four) "4個"

                                  else
                                    disabledButton
                                , if spice.canEight then
                                    button [ Events.onClick <| ChangeArea Board.Eight ] (area == Board.Eight) "8個"

                                  else
                                    disabledButton
                                ]

                            Nothing ->
                                List.repeat 4 disabledButton
                    ]
                , Html.div [ joinClasses [ "text-size-caption", "text-black55", "mb-2" ] ] [ Html.text "3. 場所を決定してください" ]
                ]
    in
    Html.div []
        [ Html.div [ Attributes.class "flex mb-4" ]
            [ Html.a [ Attributes.href "/recipes", Attributes.class "text-size-caption text-black50" ] [ Html.text "スパイスパズル一覧" ]
            , Html.div [ Attributes.class "text-size-caption text-black50 mx-2" ] [ Html.text "/" ]
            , Html.div [ Attributes.class "text-size-caption text-black50 font-bold" ] [ Html.text "作成" ]
            ]
        , if Board.completed board then
            Html.div []
                [ Html.div [ joinClasses [ "border-b", "border-black10", "mb-2" ] ]
                    [ Html.div [ joinClasses [ "my-2", "text-size-small", "font-bold" ] ] [ Html.text "コメント" ] ]
                , Html.textarea
                    [ Events.onInput InputComment
                    , Attributes.class "w-full text-size-caption p-2 border"
                    , Attributes.rows 4
                    , Attributes.placeholder "例) タンドリーチキンに合うスパイスブレンド!"
                    ]
                    [ Html.text comment ]
                , Html.div
                    [ joinClasses [ "border-b", "border-black10", "mb-2" ] ]
                    [ Html.div [ joinClasses [ "my-2", "text-size-small", "font-bold" ] ] [ Html.text "オリジナルスパイスブレンド" ] ]
                ]

          else
            questionsView
        , View.Board.view (\c -> { c | confirmSpice = Just ConfirmSpice, openDeleteModal = Just OpenDeleteModal }) board
        , if Board.completed board then
            Recipe.view board

          else
            Html.text ""
        , if Board.completed board then
            Html.div [ Attributes.class "fixed w-full pin-b pin-x flex justify-center items-center bg-white p-2 text-size-body" ]
                [ Html.button
                    [ Attributes.disabled sending
                    , Events.onClick ShareRecipe
                    , Attributes.class "bg-primary w-full text-white font-bold py-3 px-5 shadow-b"
                    ]
                    [ if sending then
                        Html.div [ Attributes.class "spinner-loader text-white" ] []

                      else
                        Html.text "みんなと共有する"
                    ]
                ]

          else
            Html.text ""
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
                                                    , checkView s.canOne
                                                    , checkView s.canTwo
                                                    , checkView s.canFour
                                                    , checkView s.canEight
                                                    ]
                                            )
                                   )
                        ]

            Nothing ->
                Html.text ""
        ]
