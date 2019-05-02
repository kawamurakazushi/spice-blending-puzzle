module Spice exposing (Spice, decoder)

import Json.Decode as Decode


type alias Spice =
    { id : Int
    , spiceName : String
    , color : String
    , oneSquare : Int
    , twoSquare : Int
    , fourSquare : Int
    , eightSquare : Int
    }


decoder : Decode.Decoder Spice
decoder =
    Decode.map7 Spice
        (Decode.field "id" Decode.int)
        (Decode.field "spiceName" Decode.string)
        (Decode.field "color" Decode.string)
        (Decode.field "oneSquare" Decode.int)
        (Decode.field "twoSquare" Decode.int)
        (Decode.field "fourSquare" Decode.int)
        (Decode.field "eightSquare" Decode.int)
