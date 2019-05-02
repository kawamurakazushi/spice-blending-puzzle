module Page.RecipeDetail exposing (Model, Msg(..), init, update, view)

import Api
import Html
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode
import Url
import Url.Builder


type alias Model =
    { recipe : Maybe Recipe
    }


type alias Recipe =
    { id : String
    , comment : String
    , created : String
    , puzzle : String
    }


init : String -> String -> ( Model, Cmd Msg )
init hash apiKey =
    let
        decoder : Decode.Decoder Recipe
        decoder =
            Decode.map4 Recipe
                (Decode.field "id" Decode.string)
                (Decode.field "comment" Decode.string)
                (Decode.field "created" Decode.string)
                (Decode.field "puzzle" Decode.string)
    in
    ( { recipe = Nothing }
    , Http.get
        { url = Api.url ++ Url.Builder.toQuery [ Url.Builder.string "resource" "recipes", Url.Builder.string "hash" hash ]
        , expect = Http.expectJson FetchedRecipe decoder
        }
    )


type Msg
    = FetchedRecipe (Result Http.Error Recipe)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedRecipe (Ok recipe) ->
            ( { model | recipe = Just recipe }, Cmd.none )

        FetchedRecipe (Err _) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [ Attributes.class "flex" ]
            [ Html.a [ Attributes.href "/recipes", Attributes.class "text-size-caption text-black50" ] [ Html.text "スパイスパズル一覧" ]
            , Html.div [ Attributes.class "text-size-caption text-black50 mx-2" ] [ Html.text "/" ]
            , Html.div [ Attributes.class "text-size-caption text-black50 font-bold" ] [ Html.text "詳細" ]
            ]
        , case model.recipe of
            Nothing ->
                Html.div [ Attributes.class "flex justify-center my-2" ]
                    [ Html.div [ Attributes.class "spinner-loader" ] []
                    ]

            Just recipe ->
                Html.div [] [ Html.text recipe.comment ]
        ]
