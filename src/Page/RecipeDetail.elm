module Page.RecipeDetail exposing (Model, Msg(..), init, update, view)

import Api
import Board
import Html
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode
import Recipe
import Spice
import Url
import Url.Builder
import View.Board
import View.Recipe


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
                Html.div []
                    [ Html.div [ Attributes.class "py-2 text-size-small font-bold border-b border-black10 mb-2" ] [ Html.text "コメント" ]
                    , Html.div [ Attributes.class "text-size-small mb-2 whitespace-pre-line" ] [ Html.text data.recipe.comment ]
                    , case Board.toBoard data.recipe.board data.spices of
                        Just b ->
                            Html.div []
                                [ Html.div [ Attributes.class "py-2 text-size-small font-bold border-b border-black10 mb-2" ] [ Html.text "オリジナルスパイスパズル" ]
                                , View.Board.view identity b
                                , View.Recipe.view b
                                ]

                        Nothing ->
                            Html.text ""
                    ]
        ]
