module Page.RecipeList exposing (Model, Msg(..), init, update, view)

import Api
import Html
import Html.Attributes as Attributes
import Http
import Json.Decode as Decode
import Url
import Url.Builder


type alias Model =
    { recipes : List Recipe
    }


type alias Recipe =
    { id : String
    , comment : String
    , created : String
    }


init : String -> ( Model, Cmd Msg )
init apiKey =
    let
        decoder : Decode.Decoder Recipe
        decoder =
            Decode.map3 Recipe
                (Decode.field "id" Decode.string)
                (Decode.field "comment" Decode.string)
                (Decode.field "created" Decode.string)
    in
    ( { recipes = [] }
    , Http.get
        { url = Api.url ++ Url.Builder.toQuery [ Url.Builder.string "resource" "recipes" ]
        , expect = Http.expectJson FetchedRecipes (Decode.list decoder)
        }
    )


type Msg
    = FetchedRecipes (Result Http.Error (List Recipe))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedRecipes (Ok recipes) ->
            ( { model | recipes = recipes }, Cmd.none )

        FetchedRecipes (Err _) ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    let
        cardView : Recipe -> Html.Html Msg
        cardView recipe =
            Html.a [ Attributes.href <| "/recipes/" ++ recipe.id, Attributes.class "no-underline bg-table-grey px-4 py-3 my-3 shadow-a block" ]
                [ Html.div [ Attributes.class "text-size-body truncate text-black90" ] [ Html.text recipe.comment ]
                , Html.div [] [ Html.div [ Attributes.class "text-size-caption text-black55" ] [ Html.text "コリアンダー" ] ]
                ]
    in
    Html.div []
        [ Html.div [ Attributes.class "text-size-caption text-black50 font-bold" ] [ Html.text "スパイスパズル一覧" ]
        , case model.recipes of
            [] ->
                Html.div [ Attributes.class "flex justify-center my-2" ]
                    [ Html.div [ Attributes.class "spinner-loader" ] []
                    ]

            recipes ->
                Html.div [] (recipes |> List.map cardView)
        ]
