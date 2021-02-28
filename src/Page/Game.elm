module Page.Game exposing (Model, Msg, init, update, view)

import Css exposing (hex, hsl, pct, px, rem)
import Css.Transitions
import Game.Cards as Cards exposing (Player(..))
import Game.Tichu as Tichu
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events as E
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy)
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
                , Css.property "display" "grid"
                , Css.property "grid-template-rows" "max-content 1fr"
                ]
            }
    in
    H.div
        [ css style.game ]
        [ viewGameHeader model
        , case model.cards of
            Dealt d ->
                viewTable d model.currentPlayers (Just Cards.South)

            Undealt ->
                H.text ""
        ]


viewGameHeader : Model -> Html Msg
viewGameHeader model =
    H.header
        [ css
            [ Css.margin (rem 1)
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.justifyContent Css.spaceBetween
            ]
        ]
        [ H.p
            [ css
                [ Css.backgroundImage
                    (Css.linearGradient2
                        Css.toRight
                        (Css.stop (hex "efa940"))
                        (Css.stop (hex "f0cb66"))
                        []
                    )
                , Css.fontSize (px 24)
                ]
            ]
            [ H.text "tichu" ]
        , button [] "Shuffle" Shuffle
        ]


viewTable : Cards.Cards Tichu.Suit -> List GamePlayer -> Maybe Cards.Player -> Html Msg
viewTable cards players currentPlayer =
    H.div
        [ css
            [ Css.property "display" "grid"
            , Css.property
                "grid-template-areas"
                """ "   .   partner   .   "
                    " left  table   right "
                    "  me     me      me  "
                """
            , Css.property "grid-template-rows" "max-content 1fr"
            , Css.property "grid-auto-rows" "max-content"
            , Css.property "grid-template-columns" "55px 1fr 55px"
            ]
        ]
        [ H.div
            [ css
                [ Css.property "grid-area" "partner" ]
            ]
            [ viewPlayerInfo cards North ]
        , H.div
            [ css
                [ Css.property "grid-area" "left" ]
            ]
            [ viewPlayerInfo cards West ]
        , H.div
            [ css
                [ Css.property "grid-area" "right" ]
            ]
            [ viewPlayerInfo cards East ]
        , H.div
            [ css
                [ Css.property "grid-area" "table"
                ]
            ]
            [ viewCardsInPlay cards players ]
        , H.div
            [ css
                [ Css.property "grid-area" "me" ]
            ]
            [ case currentPlayer of
                Just p ->
                    viewCurrentPlayer cards p

                Nothing ->
                    viewPlayerInfo cards South
            ]
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


viewPlayerInfo : Cards.Cards Tichu.Suit -> Cards.Player -> Html Msg
viewPlayerInfo cards player =
    let
        hand =
            Cards.selectFrom (Cards.PlayerLocation Cards.Hand player) cards

        faceDown =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player) cards

        taken =
            Cards.selectFrom (Cards.PlayerLocation Cards.Taken player) cards
    in
    H.div
        [ css
            (sharedStyle.player player
                ++ []
            )
        ]
        [ viewPlayerTag player
        , H.text (String.fromInt (List.length hand + List.length faceDown) ++ " in hand")
        , H.text (String.fromInt (List.length taken) ++ " taken")
        ]


viewPlayerTag : Cards.Player -> Html Msg
viewPlayerTag player =
    H.p
        [ css
            [ Css.property "background-color" "var(--c-player)"
            , Css.borderRadius (rem 0.25)
            , Css.padding (rem 0.5)
            , Css.color (hex "FFF")
            , Css.lineHeight (Css.int 1)
            , Css.fontWeight Css.bold
            ]
        ]
        [ H.text "Current player" ]


viewCurrentPlayer : Cards.Cards Tichu.Suit -> Cards.Player -> Html Msg
viewCurrentPlayer cards player =
    let
        hand =
            Cards.selectFrom (Cards.PlayerLocation Cards.Hand player) cards

        faceUp =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceUp) player) cards

        faceDown =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player) cards

        taken =
            Cards.selectFrom (Cards.PlayerLocation Cards.Taken player) cards

        style =
            { hand =
                [ Css.padding (rem 1)

                -- , Css.borderRadius (rem 1.25)
                , Css.margin (rem 1)
                , Css.position Css.relative

                -- , Css.backgroundColor (hsl 135 0.5 0.75)
                -- , Css.border3 (px 2) Css.solid (hex "FFF")
                -- , Css.boxShadow5 (px 2) (px 2) (px 20) (px -10) (hex "000")
                , Css.property "display" "grid"
                , Css.property "row-gap" "1rem"
                ]
            }
    in
    H.div
        []
        [ viewPlayerFront player ( faceUp, faceDown )
        , H.div
            [ css style.hand ]
            [ viewCurrentPlayerHeader player (List.length hand) (List.length taken) (List.length faceDown)
            , viewHand hand
            ]
        ]


viewPlayerFront : Cards.Player -> ( List (Cards.Card Tichu.Suit), List (Cards.Card Tichu.Suit) ) -> Html Msg
viewPlayerFront player ( faceUp, faceDown ) =
    H.div
        [ css
            [-- Css.marginBottom (px -100)
            ]
        ]
        [ if List.length faceDown > 0 then
            H.div
                [ css
                    [ Css.position Css.relative
                    ]
                ]
                [ H.div
                    [ css
                        [ Css.position Css.absolute
                        , Css.top (pct 66)
                        , Css.left (pct 50)
                        , Css.transform (Css.translate2 (pct -50) (pct -50))
                        ]
                    ]
                    [ button
                        [ Css.boxShadow4 (px 1) (px 1) (px 7) (hex "777")
                        ]
                        "Pick up"
                        (Action (Tichu.PickUp player))
                    ]
                , H.ol
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

          else
            H.text ""
        ]


viewHand : List (Cards.Card Tichu.Suit) -> Html Msg
viewHand hand =
    viewCardList hand


viewCurrentPlayerHeader : Cards.Player -> Int -> Int -> Int -> Html Msg
viewCurrentPlayerHeader player cardsInHand cardsTaken cardsFaceDown =
    H.div
        [ css
            (sharedStyle.player player
                ++ [ Css.displayFlex
                   , Css.alignItems Css.center
                   , Css.padding (rem 0.5)
                   , Css.border3 (px 2) Css.solid (hex "FFF")
                   , Css.borderRadius (rem 0.75)
                   , Css.property "background-color" "var(--c-player)"
                   , Css.boxShadow5 (px 2) (px 2) (px 20) (px -10) (hex "000")
                   , Css.justifyContent Css.spaceBetween
                   ]
            )
        ]
        [ viewPlayerTag player
        ]


viewCardList : List (Cards.Card Tichu.Suit) -> Html Msg
viewCardList cards =
    let
        sortedCards =
            List.sortBy (\c -> c.rank) cards
    in
    Keyed.node
        "ol"
        [ css sharedStyle.cardList ]
        (List.map
            (\card ->
                ( card.id
                , lazy
                    (\c ->
                        H.li
                            [ css sharedStyle.cardListItem ]
                            [ viewCard c
                            ]
                    )
                    card
                )
            )
            sortedCards
        )


viewCard : Cards.Card Tichu.Suit -> Html Msg
viewCard card =
    H.div
        [ css
            (sharedStyle.card
                ++ [ Css.position Css.relative
                   , Css.Transitions.transition
                        [ Css.Transitions.transform3 80 0 Css.Transitions.easeIn ]
                   , Css.hover
                        [ Css.transforms [ Css.translateY (px -10), Css.scale 1.05 ]
                        , Css.Transitions.transition
                            [ Css.Transitions.transform3 100 0 Css.Transitions.easeOut ]
                        ]
                   ]
            )
        ]
        [ H.img
            [ A.src ("/images/" ++ "tichu" ++ "/" ++ card.id ++ ".png")
            , A.alt card.fullName
            , css
                [ Css.width (pct 100)
                , Css.borderRadius Css.inherit
                ]
            ]
            []
        ]


viewFaceDownCard : Cards.Card suit -> Html Msg
viewFaceDownCard card =
    H.div
        [ css
            (sharedStyle.card
                ++ [ Css.backgroundImage
                        (Css.linearGradient2
                            (Css.deg 150)
                            (Css.stop <| hex "dadada")
                            (Css.stop2 (hex "dadada") <| pct 10)
                            [ Css.stop2 (hex "ffd9d3") <| pct 10
                            , Css.stop2 (hex "ffd9d3") <| pct 20
                            , Css.stop2 (hex "d8f1d8") <| pct 20
                            , Css.stop2 (hex "d8f1d8") <| pct 30
                            , Css.stop2 (hex "cfdfff") <| pct 30
                            , Css.stop2 (hex "cfdfff") <| pct 40
                            , Css.stop2 (hex "fcf4db") <| pct 40
                            ]
                        )
                   ]
            )
        ]
        []


button : List Css.Style -> String -> msg -> Html msg
button styles text message =
    H.button
        [ css
            ([ Css.border3 (px 2) Css.solid (hex "333")
             , Css.borderRadius (rem 0.25)
             , Css.padding (rem 0.25)
             , Css.backgroundColor (hex "d9d9d9")
             , Css.color (hex "000")
             , sharedStyle.focus
             , Css.active
                [ Css.backgroundColor (hex "eee")
                ]
             ]
                ++ styles
            )
        , E.onClick message
        ]
        [ H.text text ]


sharedStyle =
    let
        pxOverlap =
            25
    in
    { card =
        [ Css.width (px 100)
        , Css.height (px 150)
        , Css.border3 (px 1) Css.solid (hex "1b1b1b")
        , Css.boxShadow5 (px 1) (px 1) (px 10) (px -5) (hex "000")
        , Css.borderRadius (rem 0.25)
        , Css.backgroundColor (hex "#fff")
        ]
    , cardList =
        [ Css.displayFlex
        , Css.flexWrap Css.wrap
        , Css.justifyContent Css.center
        , Css.paddingLeft (px pxOverlap)
        , Css.paddingTop (px 75)
        , Css.minHeight (px 150)
        ]
    , cardListItem =
        [ Css.marginLeft (px -pxOverlap)
        , Css.marginTop (px -75)
        ]
    , focus =
        Css.focus
            [ Css.outline Css.none
            , Css.boxShadow5 Css.zero Css.zero (px 2) (px 3) (hex "4973db")
            ]
    , player =
        \player ->
            let
                playerString =
                    case player of
                        Cards.North ->
                            "north"

                        Cards.South ->
                            "south"

                        Cards.East ->
                            "east"

                        Cards.West ->
                            "west"
            in
            [ Css.property "--c-player" ("var(--c-" ++ playerString ++ ")")
            ]
    }
