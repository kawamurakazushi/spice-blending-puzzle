module Spice exposing (Spice, byId, decoder)

import Json.Decode as Decode


type alias Spice =
    { id : Int
    , name : String
    , color : String
    , canOne : Bool
    , canTwo : Bool
    , canFour : Bool
    , canEight : Bool
    }


byId : Int -> List Spice -> Maybe Spice
byId id =
    List.filter (\s -> s.id == id)
        >> List.head


decoder : Decode.Decoder Spice
decoder =
    let
        boolToIntDecoder : Decode.Decoder Bool
        boolToIntDecoder =
            let
                intToBool i =
                    if i == 1 then
                        True

                    else
                        False
            in
            Decode.map intToBool Decode.int
    in
    Decode.map7 Spice
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "color" Decode.string)
        (Decode.field "canOne" boolToIntDecoder)
        (Decode.field "canTwo" boolToIntDecoder)
        (Decode.field "canFour" boolToIntDecoder)
        (Decode.field "canEight" boolToIntDecoder)
