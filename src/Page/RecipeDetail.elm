module Page.RecipeDetail exposing (Model, Msg(..), init, update, view)

import Api
import Html
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode
import Recipe
import Spice
import Url
import Url.Builder


type alias Data =
    { recipe : Recipe.Recipe
    , spices : List Spice.Spice
    }


type alias Model =
    { data : Maybe Data
    }


init : String -> String -> ( Model, Cmd Msg )
init hash apiKey =
    ( { data = Nothing }
    , Http.get
        { url = Api.url ++ Url.Builder.toQuery [ Url.Builder.string "resource" "recipes", Url.Builder.string "hash" hash ]
        , expect =
            Http.expectJson FetchedData
                (Decode.map2 Data
                    (Decode.field "recipe" Recipe.decoder)
                    (Decode.field "spices" (Decode.list Spice.decoder))
                )
        }
    )


type Msg
    = FetchedData (Result Http.Error Data)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedData (Ok data) ->
            ( { model | data = Just data }, Cmd.none )

        FetchedData (Err _) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.div [ Attributes.class "flex" ]
            [ Html.a [ Attributes.href "/recipes", Attributes.class "text-size-caption text-black50" ] [ Html.text "スパイスパズル一覧" ]
            , Html.div [ Attributes.class "text-size-caption text-black50 mx-2" ] [ Html.text "/" ]
            , Html.div [ Attributes.class "text-size-caption text-black50 font-bold" ] [ Html.text "詳細" ]
            ]
        , case model.data of
            Nothing ->
                Html.div [ Attributes.class "flex justify-center my-2" ]
                    [ Html.div [ Attributes.class "spinner-loader" ] []
                    ]

            Just data ->
                Html.div [] [ Html.text data.recipe.comment ]
        ]
