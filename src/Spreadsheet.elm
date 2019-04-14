module Spreadsheet exposing (Key(..), SheetName(..), SpreadsheetId(..), getValues)

import Http
import Json.Decode as Decode
import Url.Builder


type Key
    = Key String


type SpreadsheetId
    = SpreadsheetId String


type SheetName
    = SheetName String


type alias Values =
    List (List String)


getValues : (Result Http.Error (List (List String)) -> msg) -> Key -> SpreadsheetId -> SheetName -> Cmd msg
getValues toMsg (Key key) (SpreadsheetId id) (SheetName name) =
    let
        a b =
            b

        decoder : Decode.Decoder Values
        decoder =
            Decode.map a
                (Decode.field "values" (Decode.list (Decode.list Decode.string)))
    in
    Http.get
        { url = Url.Builder.crossOrigin "https://sheets.googleapis.com" [ "v4", "spreadsheets", id, "values", name ++ "!A:B" ] [ Url.Builder.string "key" key ]
        , expect = Http.expectJson toMsg decoder
        }
