module Route exposing (Route(..), fromUrl)

import Url
import Url.Parser as Parser exposing ((</>))


type Route
    = CreateRecipe
    | RecipeList
    | RecipeDetail String


route : Parser.Parser (Route -> a) a
route =
    Parser.oneOf
        [ Parser.map CreateRecipe Parser.top
        , Parser.map RecipeDetail (Parser.s "recipes" </> Parser.string)
        , Parser.map RecipeList (Parser.s "recipes")
        ]


fromUrl : Url.Url -> Maybe Route
fromUrl url =
    Parser.parse route url
