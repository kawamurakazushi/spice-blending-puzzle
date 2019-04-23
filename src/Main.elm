module Main exposing (Board, Cell, Model, Msg(..), Point, Spice, Status(..), fill, range, set, update, view)

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


type alias Board =
    List (List Cell)


type alias Model =
    { startAt : Maybe Point
    , dragging : Bool
    , board : Board
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


range : Int -> Int -> List Int
range a b =
    if a > b then
        List.range b a

    else
        List.range a b


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


fill : Point -> Point -> Board -> Board
fill startPoint endPoint board =
    let
        diff end start =
            let
                d =
                    end - start
            in
            if d == 2 then
                if end == 4 then
                    1

                else
                    3

            else if d == -2 then
                if end == 1 then
                    -1

                else
                    -3

            else
                d

        xDiff =
            diff endPoint.x startPoint.x

        yDiff =
            diff endPoint.y startPoint.y

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
        |> List.foldl (\p b -> b |> set p Selected) (removeSelected board)


type Msg
    = OnMouseDown Point
    | OnMouseUp Point
    | OnMouseEnter Point
    | FetchedValues (Result Http.Error (List (List String)))
    | CloseModal
    | SelectSpice Spice


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnMouseDown point ->
            case get point model.board of
                Just (SpiceSelected _) ->
                    ( model, Cmd.none )

                _ ->
                    ( { model
                        | board =
                            model.board |> set point Selected
                        , startAt = Just point
                        , dragging = True
                      }
                    , Cmd.none
                    )

        OnMouseUp point ->
            if model.dragging then
                ( { model | dragging = False, spiceModal = True }, Cmd.none )

            else
                ( { model | dragging = False }, Cmd.none )

        OnMouseEnter point ->
            ( { model
                | board =
                    if model.dragging then
                        case get point model.board of
                            Just (SpiceSelected _) ->
                                model.board

                            _ ->
                                case model.startAt of
                                    Just startAt ->
                                        if not <| spiceInside startAt point model.board then
                                            model.board |> fill startAt point

                                        else
                                            model.board

                                    Nothing ->
                                        model.board

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
            [ Html.div [ joinClasses [ "text-size-h4", "font-secondary" ] ] [ Html.text "Spice Blending Method" ]
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
                                                 , Events.onMouseDown <| OnMouseDown point
                                                 , Events.onMouseEnter <| OnMouseEnter point
                                                 , Events.onMouseUp <| OnMouseUp point
                                                 ]
                                                    ++ (case status of
                                                            Selected ->
                                                                [ Attributes.style "background-color" "orange" ]

                                                            Blank ->
                                                                [ Attributes.style "background-color" "#fbfadfa3" ]

                                                            SpiceSelected spice ->
                                                                [ Attributes.style "background-color" spice.color ]
                                                       )
                                                )
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
