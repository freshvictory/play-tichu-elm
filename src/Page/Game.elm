module Page.Game exposing (Model, Msg, init, update, view)

import Css exposing (hex, pct, px, rem)
import Game.Game exposing (Card, CardState(..), Deck, Game, GameDeck, Location(..), PlayerHolding, PlayerLocation(..), playerHoldings)
import Game.Tichu exposing (PlayerAction(..), TichuCard, TichuDeck, TichuGame, TichuPlayer(..), TichuSuit, tichuDeck, tichuGame, tichuPlay)
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
    | TichuPlay PlayerAction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffleDeck model )

        DeckShuffled deck ->
            ( { model | deck = Dealt (dealDeck model.game deck) }, Cmd.none )

        TichuPlay play ->
            case model.deck of
                Undealt ->
                    ( model, Cmd.none )

                Dealt deck ->
                    ( { model | deck = Dealt (tichuPlay play deck) }, Cmd.none )


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
            [ E.onClick (TichuPlay (PickUp North))
            ]
            [ H.text "pick up" ]
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

        hand =
            List.filter (\c -> c.location == Hand) holdings

        ( faceUp, faceDown ) =
            List.foldl
                (\card acc ->
                    case card.location of
                        InFront FaceUp ->
                            ( card.definition :: Tuple.first acc, Tuple.second acc )

                        InFront FaceDown ->
                            ( Tuple.first acc, card.definition :: Tuple.second acc )

                        _ ->
                            acc
                )
                ( [], [] )
                holdings
    in
    H.div
        []
        [ viewPlayerFront player ( faceUp, faceDown )
        , viewHand player hand
        ]


viewPlayerFront : player -> ( List (Card suit), List (Card suit) ) -> Html Msg
viewPlayerFront player ( faceUp, faceDown ) =
    let
        style =
            { faceDown = List.append sharedStyle.cardList []
            }
    in
    H.div
        []
        [ H.ol
            [ css style.faceDown ]
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


viewHand : player -> List (PlayerHolding suit player) -> Html Msg
viewHand player hand =
    let
        sortedHand =
            List.sortBy (\c -> c.definition.rank) hand

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
                        [ viewCard card.definition
                        ]
                )
                sortedHand
            )
        ]


viewCard : Card s -> Html Msg
viewCard card =
    H.div
        [ css sharedStyle.card ]
        [ H.text card.displayName
        ]


viewFaceDownCard : Card s -> Html Msg
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
        , Css.boxShadow4 (px 2) (px 2) (px 5) (hex "#ccc")
        , Css.hover
            [ Css.borderColor (hex "5ba3dc")
            ]
        ]
    , cardList =
        [ Css.displayFlex
        , Css.flexWrap Css.wrap
        , Css.justifyContent Css.center
        , Css.paddingRight (px pxOverlap)
        ]
    , cardListItem =
        [ Css.marginLeft (px -pxOverlap) ]
    }
