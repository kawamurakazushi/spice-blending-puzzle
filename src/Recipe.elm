module Recipe exposing (Recipe, decoder)

import Board
import Json.Decode as Decode
import Spice
import Utils


type alias Recipe =
    { id : String
    , comment : String
    , created : String
    , board : String
    }


type alias RecipeWithBoard =
    { board : Board.Board
    }


decoder : Decode.Decoder Recipe
decoder =
    Decode.map4 Recipe
        (Decode.field "id" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "created" Decode.string)
        (Decode.field "board" Decode.string)
