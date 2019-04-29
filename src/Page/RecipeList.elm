module Page.RecipeList exposing (Model, Msg(..), init, update, view)

import Html


type alias Model =
    ()


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html.Html Msg
view model =
    Html.div [] [ Html.text "recipe list" ]
