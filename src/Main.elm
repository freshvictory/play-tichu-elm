port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html.Styled as Html
import Page
import Page.Design as Design
import Page.Game as Game
import Page.Home as Home
import Url
import Url.Parser as Parser
    exposing
        ( (</>)
        , Parser
        , oneOf
        , s
        , top
        )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


port blurActiveElement : () -> Cmd msg



---- MODEL ----


type alias Model =
    { key : Nav.Key
    , page : Page
    }


type Page
    = NotFound
    | Home Home.Model
    | Game Game.Model
    | Design Design.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    routeToUrl url { key = key, page = NotFound }



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | GameMsg Game.Msg
    | DesignMsg Design.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Cmd.batch
                        [ blurActiveElement ()
                        , Nav.pushUrl model.key (Url.toString url)
                        ]
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            routeToUrl url model

        ( HomeMsg m, Home h ) ->
            routeHome model (Home.update m h)

        ( GameMsg m, Game g ) ->
            routeGame model (Game.update m g)

        ( DesignMsg m, Design d ) ->
            routeDesign model (Design.update m d)

        _ ->
            ( model, Cmd.none )


routeHome : Model -> ( Home.Model, Cmd Home.Msg ) -> ( Model, Cmd Msg )
routeHome model ( home, m ) =
    ( { model | page = Home home }, Cmd.map HomeMsg m )


routeGame : Model -> ( Game.Model, Cmd Game.Msg ) -> ( Model, Cmd Msg )
routeGame model ( game, m ) =
    ( { model | page = Game game }, Cmd.map GameMsg m )


routeDesign : Model -> ( Design.Model, Cmd Design.Msg ) -> ( Model, Cmd Msg )
routeDesign model ( design, m ) =
    ( { model | page = Design design }, Cmd.map DesignMsg m )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound ->
            Page.view never
                { title = "Not found"
                , attrs = []
                , body = [ Html.text "Page not found." ]
                }

        Home home ->
            Page.view HomeMsg (Home.view home)

        Game game ->
            Page.view GameMsg (Game.view game)

        Design design ->
            Page.view DesignMsg (Design.view design)



-- ROUTER


routeToUrl : Url.Url -> Model -> ( Model, Cmd Msg )
routeToUrl url model =
    let
        parser =
            oneOf
                [ route top
                    (routeHome model Home.init)
                , route (s "index.html")
                    (routeHome model Home.init)
                , route (s "game" </> Parser.string)
                    (\gameId -> routeGame model (Game.init gameId))
                , route (s "design")
                    (routeDesign model Design.init)
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }, Cmd.none )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
