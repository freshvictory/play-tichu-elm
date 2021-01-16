module Page.Game exposing (Model, Msg, init, update, view)

import Game.Game exposing (GameDeck, Location(..), PlayerLocation(..), playerHoldings)
import Game.Tichu exposing (TichuCard, TichuDeck, TichuGame, TichuPlayer(..), TichuSuit, tichuDeck, tichuGame)
import Html.Attributes exposing (list)
import Html.Styled as H exposing (Html)
import Html.Styled.Events as E
import Page
import Random



-- MODEL


type alias DealtDeck =
    GameDeck TichuSuit TichuPlayer


type CurrentDeck
    = Undealt
    | Dealt DealtDeck


type alias Model =
    { gameId : String
    , game : TichuGame
    , deck : CurrentDeck
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


dealDeck : TichuGame -> TichuDeck -> DealtDeck
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


viewPlayer : DealtDeck -> TichuPlayer -> Html Msg
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
                        [ H.text card.definition.displayName
                        ]
                )
                holdings
            )
        ]


viewCard : TichuCard -> Html Msg
viewCard card =
    H.div
        []
        [ H.text card.displayName
        ]
