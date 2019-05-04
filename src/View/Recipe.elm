module View.Recipe exposing (view)

import Board
import Html
import Html.Attributes as Attributes


view : Board.Board -> Html.Html msg
view board =
    Html.div []
        [ Html.div [ Attributes.class "border-b border-black10 mb-2" ]
            [ Html.div [ Attributes.class "my-2 text-size-small font-bold" ] [ Html.text "レシピ (4人分)" ] ]
        , Html.div [ Attributes.style "display" "grid", Attributes.style "grid-template-columns" "1fr 100px" ]
            (board
                |> List.map
                    (\cell ->
                        case cell.status of
                            Board.SpiceSelected spice area ->
                                [ Html.div [ Attributes.class "my-1 text-size-small" ] [ Html.text spice.name ]
                                , Html.div [ Attributes.class "my-1 text-size-small" ] [ Html.text <| "小さじ " ++ (cell |> Board.cells |> List.length |> toFloat |> (*) 0.5 |> String.fromFloat) ]
                                ]

                            _ ->
                                []
                    )
                |> List.foldl (++) []
            )
        ]
