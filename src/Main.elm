module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode
import List.Extra
import Page.CreateRecipe as CreateRecipe
import Page.RecipeDetail as RecipeDetail
import Page.RecipeList as RecipeList
import Route
import Url


type PageModel
    = CreateRecipeModel CreateRecipe.Model
    | RecipeListModel RecipeList.Model
    | RecipeDetailModel RecipeDetail.Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , apiKey : String
    , page : PageModel
    }


urlToPage : Nav.Key -> String -> Url.Url -> ( PageModel, Cmd Msg )
urlToPage key apiKey url =
    case Route.fromUrl url of
        Just Route.RecipeList ->
            RecipeList.init apiKey
                |> Tuple.mapFirst RecipeListModel
                |> Tuple.mapSecond (Cmd.map RecipeListMsg)

        Just (Route.RecipeDetail hash) ->
            RecipeDetail.init hash apiKey
                |> Tuple.mapFirst RecipeDetailModel
                |> Tuple.mapSecond (Cmd.map RecipeDetailMsg)

        Just Route.CreateRecipe ->
            CreateRecipe.init key apiKey
                |> Tuple.mapFirst CreateRecipeModel
                |> Tuple.mapSecond (Cmd.map CreateRecipeMsg)

        Nothing ->
            CreateRecipe.init key apiKey
                |> Tuple.mapFirst CreateRecipeModel
                |> Tuple.mapSecond (Cmd.map CreateRecipeMsg)


init : a -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        apiKey =
            "AIzaSyAF76ebBsSdMWFFbL4jU6w3qbETWWjYzTo"

        ( page, cmd ) =
            urlToPage key apiKey url
    in
    ( { url = url
      , key = key
      , apiKey = apiKey
      , page = page
      }
    , cmd
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CreateRecipeMsg CreateRecipe.Msg
    | RecipeListMsg RecipeList.Msg
    | RecipeDetailMsg RecipeDetail.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                ( page, cmd ) =
                    urlToPage model.key model.apiKey url
            in
            ( { model | url = url, page = page }
            , cmd
            )

        CreateRecipeMsg subMsg ->
            case model.page of
                CreateRecipeModel subModel ->
                    let
                        ( newModel, newCmd ) =
                            CreateRecipe.update subMsg subModel
                    in
                    ( { model
                        | page =
                            CreateRecipeModel newModel
                      }
                    , newCmd |> Cmd.map CreateRecipeMsg
                    )

                _ ->
                    ( model, Cmd.none )

        RecipeListMsg subMsg ->
            case model.page of
                RecipeListModel subModel ->
                    let
                        ( newModel, newCmd ) =
                            RecipeList.update subMsg subModel
                    in
                    ( { model
                        | page =
                            RecipeListModel newModel
                      }
                    , newCmd |> Cmd.map RecipeListMsg
                    )

                _ ->
                    ( model, Cmd.none )

        RecipeDetailMsg subMsg ->
            case model.page of
                RecipeDetailModel subModel ->
                    let
                        ( newModel, newCmd ) =
                            RecipeDetail.update subMsg subModel
                    in
                    ( { model
                        | page =
                            RecipeDetailModel newModel
                      }
                    , newCmd |> Cmd.map RecipeDetailMsg
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Browser.Document Msg
view { page } =
    let
        layout child =
            Html.div [ Attributes.class "flex justify-center", Attributes.style "padding-bottom" "100px" ]
                [ Html.div [ Attributes.class "max-w-content w-full flex flex-col justify-center p-4" ]
                    [ Html.div [ Attributes.class "flex items-center mb-2" ]
                        [ Html.div [ Attributes.class "flex-1 text-size-h5 font-secondary mb-1" ]
                            [ Html.text "スパイス・ブレンディング・パズル" ]
                        ]
                    , child
                    , case page of
                        CreateRecipeModel _ ->
                            Html.text ""

                        _ ->
                            Html.a
                                [ Attributes.href "/"
                                , Attributes.class "no-underline w-box h-box rounded-full bg-primary fixed text-white pin-b pin-r m-5 shadow-c block flex flex-col items-center justify-center"
                                ]
                                [ Html.i [ Attributes.class "fa fa-plus text-size-h5" ] [] ]
                    ]
                ]
    in
    { title = "スパイスブレンディングパズル"
    , body =
        [ layout <|
            case page of
                CreateRecipeModel model ->
                    CreateRecipe.view model
                        |> Html.map CreateRecipeMsg

                RecipeListModel model ->
                    RecipeList.view model
                        |> Html.map RecipeListMsg

                RecipeDetailModel model ->
                    RecipeDetail.view model
                        |> Html.map RecipeDetailMsg
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program String Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }
