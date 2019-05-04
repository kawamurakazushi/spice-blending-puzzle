module Recipe exposing (Recipe, decoder)

import Board
import Json.Decode as Decode
import Spice


type alias Recipe =
    { id : String
    , comment : String
    , created : String
    , puzzle : String
    }


type alias RecipeWithBoard =
    { board : Board.Board
    }


withBoard : Recipe -> List Spice.Spice -> RecipeWithBoard
withBoard recipe spice =
    { board = Board.initialBoard
    }


decoder : Decode.Decoder Recipe
decoder =
    Decode.map4 Recipe
        (Decode.field "id" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "created" Decode.string)
        (Decode.field "puzzle" Decode.string)
