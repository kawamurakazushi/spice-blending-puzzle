module View.Board exposing (view)

import Board
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Spice


type alias OptionalConfig msg =
    { confirmSpice : Maybe msg
    , openDeleteModal : Maybe (Spice.Spice -> msg)
    }


defaultOptionalConfig : OptionalConfig msg
defaultOptionalConfig =
    { confirmSpice = Nothing
    , openDeleteModal = Nothing
    }


view : (OptionalConfig msg -> OptionalConfig msg) -> Board.Board -> Html.Html msg
view f board =
    let
        { confirmSpice, openDeleteModal } =
            f defaultOptionalConfig
    in
    Html.div [ Attributes.style "display" "grid", Attributes.style "grid-template-rows" "92px 92px 92px 92px", Attributes.style "grid-template-columns" "1fr 1fr 1fr 1fr " ] <|
        (board
            |> List.map
                (\{ row, col, status } ->
                    Html.div
                        ([ Attributes.style "grid-row"
                            ((row
                                |> Tuple.first
                                |> String.fromInt
                             )
                                ++ "/"
                                ++ (row |> Tuple.second |> String.fromInt)
                            )
                         , Attributes.style "grid-column"
                            ((col
                                |> Tuple.first
                                |> String.fromInt
                             )
                                ++ "/"
                                ++ (col |> Tuple.second |> String.fromInt)
                            )
                         , Attributes.class <|
                            "border border-white text-size-caption font-bold flex justify-center items-center"
                                ++ (case status of
                                        Board.Selected ->
                                            " z-10 shadow-b"

                                        Board.SpiceSelected _ _ ->
                                            ""

                                        Board.Blank ->
                                            " bg-black10"
                                   )
                         ]
                            ++ (case status of
                                    Board.Selected ->
                                        [ Attributes.style "outline" "3px solid oldlace"
                                        ]
                                            ++ (case confirmSpice of
                                                    Just c ->
                                                        [ Events.onClick c ]

                                                    Nothing ->
                                                        []
                                               )

                                    Board.SpiceSelected spice _ ->
                                        [ Attributes.style "background-color" (spice.color ++ "70")
                                        ]
                                            ++ (case openDeleteModal of
                                                    Just c ->
                                                        [ Events.onClick <| c spice ]

                                                    Nothing ->
                                                        []
                                               )

                                    _ ->
                                        []
                               )
                        )
                        (case status of
                            Board.Selected ->
                                [ Html.div [ Attributes.class "rounded-full p-3 shadow-a", Attributes.style "background-color" "oldlace" ] [ Html.text "決定" ] ]

                            Board.SpiceSelected spice _ ->
                                [ Html.text spice.name ]

                            _ ->
                                [ Html.text "" ]
                        )
                )
        )
