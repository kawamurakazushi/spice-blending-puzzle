module Recipe exposing (Recipe, decoder)

import Json.Decode as Decode


type alias Recipe =
    { id : String
    , comment : String
    , created : String
    , puzzle : String
    }


decoder : Decode.Decoder Recipe
decoder =
    Decode.map4 Recipe
        (Decode.field "id" Decode.string)
        (Decode.field "comment" Decode.string)
        (Decode.field "created" Decode.string)
        (Decode.field "puzzle" Decode.string)
