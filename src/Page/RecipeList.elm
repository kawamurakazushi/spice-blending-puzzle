module Page.RecipeList exposing (Model, Msg(..), init, update, view)

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
    { recipes : List Recipe.Recipe
    , spices : List Spice.Spice
    }


type alias Model =
    { data : Data }


init : String -> ( Model, Cmd Msg )
init apiKey =
    ( { data = { recipes = [], spices = [] } }
    , Http.get
        { url = Api.url ++ Url.Builder.toQuery [ Url.Builder.string "resource" "recipes" ]
        , expect =
            Http.expectJson FetchedData
                (Decode.map2 Data
                    (Decode.field "recipes" (Decode.list Recipe.decoder))
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
            ( { model | data = data }, Cmd.none )

        FetchedData (Err _) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        cardView : Recipe.Recipe -> Html.Html Msg
        cardView recipe =
            Html.a [ Attributes.href <| "/recipes/" ++ recipe.id, Attributes.class "no-underline bg-table-grey px-4 py-3 my-3 shadow-a block" ]
                [ Html.div [ Attributes.class "text-size-body truncate text-black90" ] [ Html.text recipe.comment ]
                , Html.div [] [ Html.div [ Attributes.class "text-size-caption text-black55" ] [ Html.text "コリアンダー" ] ]
                ]
    in
    Html.div []
        [ Html.div [ Attributes.class "text-size-caption text-black50 font-bold" ] [ Html.text "スパイスパズル一覧" ]
        , case model.data.recipes of
            [] ->
                Html.div [ Attributes.class "flex justify-center my-2" ]
                    [ Html.div [ Attributes.class "spinner-loader" ] []
                    ]

            recipes ->
                Html.div [] (recipes |> List.map cardView)
        ]
