module Board exposing
    ( Area(..)
    , Board
    , Cell
    , Status(..)
    , cells
    , completed
    , confirmSpice
    , include
    , initialBoard
    , remove
    , selected
    )

import Spice
import Utils


type Area
    = One
    | Two
    | Four
    | Eight


type Status
    = Selected
    | Blank
    | SpiceSelected Spice.Spice Area


type alias Cell =
    { row : ( Int, Int )
    , col : ( Int, Int )
    , status : Status
    }


type alias Board =
    List Cell


initialBoard : Board
initialBoard =
    let
        grid =
            List.range 1 4 |> List.map (\i -> ( i, i + 1 ))
    in
    grid
        |> List.map
            (\y ->
                grid
                    |> List.reverse
                    |> List.map (\x -> { row = x, col = y, status = Blank })
            )
        |> List.foldl (++) []


selected : Area -> Board -> Board
selected area board =
    let
        blanks =
            board
                |> remove Selected
                |> List.filter (\{ status } -> status == Blank)
                |> List.sortBy (\cell -> cell.row |> Tuple.first)
                |> List.sortBy (\cell -> cell.col |> Tuple.first)
                |> List.reverse

        mCell =
            blanks
                |> List.foldl
                    (\cell mergedCells ->
                        mergedCells
                            ++ (case area of
                                    One ->
                                        [ cell ]

                                    Two ->
                                        [ { cell
                                            | row =
                                                cell.row
                                                    |> Tuple.mapSecond ((+) 1)
                                          }
                                        , { cell
                                            | col =
                                                cell.col
                                                    |> Tuple.mapSecond ((+) 1)
                                          }
                                        ]

                                    Four ->
                                        [ { cell
                                            | col =
                                                cell.col
                                                    |> Tuple.mapSecond ((+) 1)
                                            , row =
                                                cell.row
                                                    |> Tuple.mapSecond ((+) 1)
                                          }
                                        ]

                                    Eight ->
                                        [ { cell
                                            | col =
                                                cell.col
                                                    |> Tuple.mapSecond ((+) 1)
                                            , row =
                                                cell.row
                                                    |> Tuple.mapSecond ((+) 3)
                                          }
                                        ]
                               )
                    )
                    []
                |> List.foldl
                    (\mergedCell point ->
                        if cells mergedCell |> List.all (Utils.flip List.member blanks) then
                            Just mergedCell

                        else
                            point
                    )
                    Nothing
    in
    case mCell of
        Just cell ->
            board
                |> remove Selected
                |> removeCells (cells cell)
                |> (++) [ { cell | status = Selected } ]

        Nothing ->
            board


cells : Cell -> List Cell
cells cell =
    let
        ( minRow, maxRow ) =
            cell.row

        ( minCol, maxCol ) =
            cell.col

        rangeRow =
            List.range minRow (maxRow - 1) |> List.map (\a -> ( a, a + 1 ))

        rangeCol =
            List.range minCol (maxCol - 1) |> List.map (\a -> ( a, a + 1 ))
    in
    rangeRow
        |> List.map
            (\y ->
                rangeCol
                    |> List.map (\x -> { row = y, col = x, status = cell.status })
            )
        |> List.foldr (++) []


removeCells : List Cell -> Board -> Board
removeCells cellList board =
    List.foldl (\cell b -> b |> List.filter ((/=) cell)) board cellList


confirmSpice : Spice.Spice -> Board -> Board
confirmSpice spice board =
    board
        |> List.map
            (\cell ->
                if cell.status == Selected then
                    { cell | status = SpiceSelected spice One }

                else
                    cell
            )


remove : Status -> Board -> Board
remove status board =
    let
        mCell =
            board
                |> List.filter
                    (\c -> c.status == status)
                |> List.head
    in
    case mCell of
        Just cell ->
            board
                |> removeCells [ cell ]
                |> (++) (cells { cell | status = Blank })

        Nothing ->
            board


include : Spice.Spice -> Board -> Bool
include spice =
    List.foldl
        (\cell b ->
            case cell.status of
                SpiceSelected s _ ->
                    if s.id == spice.id then
                        True

                    else
                        b

                _ ->
                    b
        )
        False


completed : Board -> Bool
completed =
    List.foldl
        (\{ status } b ->
            if status == Blank || status == Selected then
                False

            else
                b
        )
        True
