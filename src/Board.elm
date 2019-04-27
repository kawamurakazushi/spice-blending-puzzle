module Board exposing
    ( Area(..)
    , Board
    , Cell
    , Point
    , Spice
    , Status(..)
    , firstSelected
    , include
    , initialBoard
    , removeSelected
    , spiceInside
    )


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


type alias Point =
    { x : Int
    , y : Int
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


initialBoard : Board
initialBoard =
    List.range 1 4
        |> List.map
            (\y ->
                List.range 1 4
                    |> List.map
                        (\x -> Cell (Point x y) Blank)
            )


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
get point =
    List.foldr (++) []
        >> List.foldl
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


include : Spice -> Board -> Bool
include spice =
    List.foldr (++) []
        >> List.foldl
            (\cell b ->
                case cell.status of
                    SpiceSelected s ->
                        if s == spice then
                            True

                        else
                            b

                    _ ->
                        b
            )
            False
