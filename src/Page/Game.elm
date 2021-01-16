module Page.Game exposing (Model, Msg, init, update, view)

import Css
import Game.Game exposing (Card, Deck, Game, GameDeck, Location(..), PlayerLocation(..), playerHoldings)
import Game.Tichu exposing (TichuCard, TichuDeck, TichuGame, TichuPlayer(..), TichuSuit, tichuDeck, tichuGame)
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes exposing (css, list)
import Html.Styled.Events as E
import Page
import Random



-- MODEL


type alias DealtDeck suit player =
    GameDeck suit player


type CurrentDeck suit player
    = Undealt
    | Dealt (DealtDeck suit player)


type alias Model =
    { gameId : String
    , game : TichuGame
    , deck : CurrentDeck TichuSuit TichuPlayer
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId, game = tichuGame, deck = Undealt }, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Shuffle
    | DeckShuffled TichuDeck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffleDeck model )

        DeckShuffled deck ->
            ( { model | deck = Dealt (dealDeck model.game deck) }, Cmd.none )


shuffleDeck : Model -> Cmd Msg
shuffleDeck model =
    Random.generate DeckShuffled model.game.shuffledDeckGenerator


dealDeck : Game suit player -> Deck suit -> DealtDeck suit player
dealDeck game deck =
    game.deal deck



-- VIEW


view : Model -> Page.Details Msg
view model =
    { title = "Tichu Game"
    , attrs = []
    , body =
        [ viewGame model
        ]
    }


viewGame : Model -> Html Msg
viewGame model =
    H.div
        []
        [ H.text ("Hi, it's tichu!" ++ model.gameId)
        , H.button
            [ E.onClick Shuffle
            ]
            [ H.text "Shuffle!"
            ]
        , case model.deck of
            Dealt d ->
                viewPlayer d North

            Undealt ->
                H.text ""
        ]


viewPlayer : DealtDeck suit player -> player -> Html Msg
viewPlayer deck player =
    let
        holdings =
            playerHoldings deck player
    in
    H.div
        []
        [ H.ol
            []
            (List.map
                (\card ->
                    H.li
                        []
                        [ viewCard card.definition
                        ]
                )
                holdings
            )
        ]


viewCard : Card s -> Html Msg
viewCard card =
    let
        style =
            [ Css.width (Css.px 100)
            , Css.height (Css.px 150)
            , Css.border3 (Css.px 1) Css.solid (Css.hex "#000")
            , Css.borderRadius (Css.px 5)
            , Css.backgroundColor (Css.hex "#fff")
            ]
    in
    H.div
        [ css style ]
        [ H.text card.displayName
        ]
