module Page.Game exposing (Model, Msg, init, update, view)

import Game.Tichu exposing (TichuDeck, TichuGame, tichuGame)
import Html.Styled as H exposing (Html)
import Page
import Random
import Game.Tichu exposing (tichuDeck)



-- MODEL


type alias Model =
    { gameId : String
    , game : TichuGame
    , deck : TichuDeck
    }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId, game = tichuGame, deck = tichuDeck }, Cmd.none )



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
            ( model, Random.generate DeckShuffled model.game.shuffledDeckGenerator)

        DeckShuffled deck ->
            ( { model | deck = deck }, Cmd.none )



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
        ]


viewHands : Model -> Html Msg
viewHands model =
    H.ol
        []
        ( List.map
            (\card ->
                H.li
                    []
                    [ H.text ("Card " ++ card.name)
                    ]
            )
            model.deck
        )
    
