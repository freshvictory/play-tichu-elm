module Page.Game exposing (Model, Msg, init, update, view)

import Css exposing (hex, pct, px, rem)
import Game.Cards as Cards
import Game.Tichu as Tichu
import Html.Styled as H exposing (Html, i)
import Html.Styled.Attributes exposing (css, list)
import Html.Styled.Events as E
import Page
import Random



-- MODEL


type Cards suit
    = Undealt
    | Dealt (Cards.Cards suit)


type alias GamePlayer =
    { id : String
    , name : String
    , player : Cards.Player
    }


type alias Model =
    { gameId : String
    , game : Cards.Game Tichu.Suit Tichu.Action
    , cards : Cards Tichu.Suit
    , currentPlayers : List GamePlayer
    }


currentPlayers : List GamePlayer
currentPlayers =
    List.indexedMap
        (\i p ->
            { id = String.fromInt i
            , name = String.fromInt i
            , player = p
            }
        )
        [ Cards.North, Cards.East, Cards.South, Cards.West ]


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , game = Cards.buildGame Tichu.gameDefinition
      , cards = Undealt
      , currentPlayers = currentPlayers
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Shuffle
    | DeckShuffled (Cards.Deck Tichu.Suit)
    | Action Tichu.Action


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffleDeck model.game.deck )

        DeckShuffled cards ->
            ( { model | cards = Dealt (model.game.deal cards) }, Cmd.none )

        Action action ->
            case model.cards of
                Undealt ->
                    ( model, Cmd.none )

                Dealt cards ->
                    ( { model | cards = Dealt (model.game.act action cards) }, Cmd.none )


shuffleDeck : Cards.Deck Tichu.Suit -> Cmd Msg
shuffleDeck deck =
    Random.generate DeckShuffled (Cards.shuffle deck)



-- VIEW


view : Model -> Page.Details Msg
view model =
    { title = "Tichu Game"
    , attrs = [ css [ Css.height (pct 100) ] ]
    , body =
        [ viewGame model
        ]
    }


viewGame : Model -> Html Msg
viewGame model =
    let
        style =
            { game =
                [ Css.property "background-color" "var(--c-table)"
                , Css.height (pct 100)
                ]
            }
    in
    H.div
        [ css style.game ]
        [ H.text ("Hi, it's tichu!" ++ model.gameId)
        , H.button
            [ E.onClick Shuffle
            ]
            [ H.text "Shuffle!"
            ]
        , H.button
            [ E.onClick (Action (Tichu.PickUp Cards.North))
            ]
            [ H.text "pick up" ]
        , case model.cards of
            Dealt d ->
                viewTable d model.currentPlayers (Just Cards.North)

            Undealt ->
                H.text ""
        ]


viewTable : Cards.Cards Tichu.Suit -> List GamePlayer -> Maybe Cards.Player -> Html Msg
viewTable cards players currentPlayer =
    H.div
        []
        [ viewCardsInPlay cards players
        , case currentPlayer of
            Just p ->
                viewCurrentPlayer cards p

            Nothing ->
                H.text ""
        ]


viewCardsInPlay : Cards.Cards Tichu.Suit -> List GamePlayer -> Html Msg
viewCardsInPlay cards players =
    let
        cardsOnTable =
            Cards.byPlay Cards.Table cards
    in
    H.div
        []
        (List.map
            (\( player, cardsInPlay ) ->
                H.ol
                    [ css sharedStyle.cardList ]
                    (List.map
                        (\card ->
                            H.li
                                [ css sharedStyle.cardListItem ]
                                [ viewCard card
                                ]
                        )
                        cardsInPlay
                    )
            )
            cardsOnTable
        )


viewCurrentPlayer : Cards.Cards Tichu.Suit -> Cards.Player -> Html Msg
viewCurrentPlayer cards player =
    let
        hand =
            Cards.selectFrom (Cards.PlayerLocation Cards.Hand player) cards

        faceUp =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceUp) player) cards

        faceDown =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player) cards
    in
    H.div
        []
        [ viewPlayerFront player ( faceUp, faceDown )
        , viewHand player hand
        , H.button
            [ E.onClick (Action (Tichu.Play player (Tichu.Straight hand)))
            ]
            [ H.text "Play" ]
        ]


viewPlayerFront : player -> ( List (Cards.Card suit), List (Cards.Card suit) ) -> Html Msg
viewPlayerFront player ( faceUp, faceDown ) =
    H.div
        []
        [ H.ol
            [ css sharedStyle.cardList ]
            (List.map
                (\card ->
                    H.li
                        [ css sharedStyle.cardListItem ]
                        [ viewFaceDownCard card
                        ]
                )
                faceDown
            )
        ]


viewHand : player -> List (Cards.Card suit) -> Html Msg
viewHand player hand =
    let
        sortedHand =
            List.sortBy (\c -> c.rank) hand

        style =
            { hand = List.append sharedStyle.cardList []
            , cardContainer = List.append sharedStyle.cardListItem []
            }
    in
    H.div
        []
        [ H.ol
            [ css style.hand ]
            (List.map
                (\card ->
                    H.li
                        [ css style.cardContainer ]
                        [ viewCard card
                        ]
                )
                sortedHand
            )
        ]


viewCard : Cards.Card suit -> Html Msg
viewCard card =
    H.div
        [ css sharedStyle.card ]
        [ H.text card.displayName
        ]


viewFaceDownCard : Cards.Card suit -> Html Msg
viewFaceDownCard card =
    H.div
        [ css sharedStyle.card ]
        []


sharedStyle =
    let
        pxOverlap =
            50
    in
    { card =
        [ Css.width (px 100)
        , Css.height (px 150)
        , Css.border3 (px 1) Css.solid (hex "#000")
        , Css.borderRadius (px 5)
        , Css.backgroundColor (hex "#fff")
        , Css.boxShadow5 (px 1) (px 1) (px 10) (px -4) (hex "#333")
        , Css.padding (px 5)
        ]
    , cardList =
        [ Css.displayFlex
        , Css.flexWrap Css.wrap
        , Css.justifyContent Css.center
        , Css.paddingLeft (px pxOverlap)
        , Css.paddingTop (px 75)
        ]
    , cardListItem =
        [ Css.marginLeft (px -pxOverlap)
        , Css.marginTop (px -75)
        ]
    }
