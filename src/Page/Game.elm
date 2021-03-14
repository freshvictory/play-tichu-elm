module Page.Game exposing (Model, Msg, init, update, view)

import Css exposing (pct, px, rem)
import Css.Transitions
import Design
import Game.Cards as Cards
import Game.Players as Players exposing (Player(..), Players)
import Game.Tichu as Tichu
import Html.Styled as H exposing (Html)
import Html.Styled.Attributes as A exposing (css)
import Html.Styled.Events as E
import Html.Styled.Keyed as Keyed
import Html.Styled.Lazy exposing (lazy)
import Page
import Random
import Set exposing (Set)
import Svg
import Svg.Styled exposing (Svg)



-- MODEL


type Game suit
    = Undealt
    | Dealt Tichu.Game


type alias GamePlayer =
    { id : String
    , name : String
    , player : Player
    }


type alias Model =
    { gameId : String
    , deck : Cards.PlayableDeck Tichu.Suit
    , game : Game Tichu.Suit
    , currentPlayers : Players GamePlayer
    , selectedCards : Set String
    }


currentPlayers : Players GamePlayer
currentPlayers =
    Players.fromMap
        { north = { id = "1", name = "Lyle", player = North }
        , south = { id = "2", name = "Lydia", player = South }
        , west = { id = "3", name = "Nick", player = West }
        , east = { id = "4", name = "Justin", player = East }
        }


init : String -> ( Model, Cmd Msg )
init gameId =
    ( { gameId = gameId
      , deck = Cards.buildDeck Tichu.deckDefinition
      , game = Undealt
      , currentPlayers = currentPlayers
      , selectedCards = Set.empty
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Shuffle
    | DeckShuffled (Cards.Deck Tichu.Suit)
    | Action Tichu.Action
    | ToggleCard String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffleDeck model.deck.deck )

        DeckShuffled cards ->
            ( { model | game = Dealt (Tichu.newGame (model.deck.deal cards)), selectedCards = Set.empty }, Cmd.none )

        Action action ->
            case model.game of
                Undealt ->
                    ( model, Cmd.none )

                Dealt game ->
                    ( { model | game = Dealt (Tichu.act action game) }, Cmd.none )

        ToggleCard id checked ->
            ( { model
                | selectedCards =
                    if checked then
                        Set.insert id model.selectedCards

                    else
                        Set.remove id model.selectedCards
              }
            , Cmd.none
            )


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
                [ Css.backgroundColor Design.color.table
                , Css.height (pct 100)
                , Css.property "display" "grid"
                , Css.property "grid-template-rows" "max-content 1fr"
                ]
            }
    in
    H.div
        [ css style.game ]
        [ viewGameHeader model
        , case model.game of
            Dealt game ->
                viewTable model game model.currentPlayers (Just South)

            Undealt ->
                H.text ""
        ]


viewGameHeader : Model -> Html Msg
viewGameHeader model =
    H.header
        [ css
            [ Css.margin Design.spacing.medium
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
                        (Css.stop (Css.hex "efa940"))
                        (Css.stop (Css.hex "f0cb66"))
                        []
                    )
                , Css.fontSize (px 24)
                ]
            ]
            [ H.text "tichu" ]
        , Design.button.secondary "Shuffle" Shuffle []
        ]


viewTable : Model -> Tichu.Game -> Players GamePlayer -> Maybe Player -> Html Msg
viewTable model game players currentPlayer =
    let
        order =
            case currentPlayer of
                Just p ->
                    { top = Players.partner p
                    , left = Players.next p
                    , bottom = p
                    , right = Players.previous p
                    }

                Nothing ->
                    { top = North
                    , left = East
                    , bottom = South
                    , right = West
                    }
    in
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
            , Css.margin Design.spacing.medium
            ]
        ]
        [ H.div
            [ css
                [ Css.property "grid-area" "partner"
                , Css.displayFlex
                , Css.justifyContent Css.spaceAround
                ]
            ]
            [ viewPlayerInfo game (Players.get order.top players) ]
        , H.div
            [ css
                [ Css.property "grid-area" "left"
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.spaceAround
                , Css.alignItems Css.center
                ]
            ]
            [ H.div
                [ css
                    [ Css.position Css.relative
                    , Css.property "width" "max-content"
                    , Css.transform (Css.rotate (Css.deg -90))
                    ]
                ]
                [ viewPlayerInfo game (Players.get order.left players) ]
            ]
        , H.div
            [ css
                [ Css.property "grid-area" "right"
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.justifyContent Css.spaceAround
                , Css.alignItems Css.center
                ]
            ]
            [ H.div
                [ css
                    [ Css.position Css.relative
                    , Css.property "width" "max-content"
                    , Css.transform (Css.rotate (Css.deg 90))
                    ]
                ]
                [ viewPlayerInfo game (Players.get order.right players) ]
            ]
        , H.div
            [ css
                [ Css.property "grid-area" "table"
                ]
            ]
            [ viewCardsInPlay game players ]
        , H.div
            [ css
                [ Css.property "grid-area" "me" ]
            ]
            [ case currentPlayer of
                Just p ->
                    viewPlayer model game (Players.get p players)

                Nothing ->
                    viewPlayerInfo game (Players.get order.bottom players)
            ]
        ]


viewCardsInPlay : Tichu.Game -> Players GamePlayer -> Html Msg
viewCardsInPlay game players =
    let
        cardsOnTable =
            Cards.byPlay Cards.Table game.cards
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


viewPlayerInfo : Tichu.Game -> GamePlayer -> Html Msg
viewPlayerInfo game player =
    let
        hand =
            Cards.selectFrom (Cards.PlayerLocation Cards.Hand player.player) game.cards

        taken =
            Cards.selectFrom (Cards.PlayerLocation Cards.Taken player.player) game.cards
    in
    H.div
        [ css
            (sharedStyle.player player.player
                ++ [ Css.property "display" "grid"
                   , Css.property "grid-auto-flow" "column"
                   , Css.property "column-gap" Design.spacing.medium.value
                   , Css.alignItems Css.center
                   , Css.padding Design.spacing.small
                   , Css.borderRadius Design.borderRadius.outer
                   , Css.backgroundColor Design.color.lightTable
                   , Design.shadow.underscore
                   , Css.justifyContent Css.spaceBetween
                   , Css.margin Css.auto
                   , Css.property "width" "max-content"
                   ]
            )
        ]
        [ viewPlayerTag player
        , H.span
            [ css
                [ Svg.primaryColor Design.color.lightPrimary.value
                ]
            ]
            [ svgWithText Svg.hand (String.fromInt (List.length hand)) ]
        , H.span
            [ css
                [ Svg.primaryColor Design.color.lightPrimary.value
                ]
            ]
            [ svgWithText Svg.stack (String.fromInt (List.length taken)) ]
        , viewBet (Players.get player.player game.bets)
        ]


viewPlayerTag : GamePlayer -> Html Msg
viewPlayerTag player =
    H.p
        [ css
            [ Css.property "background-color" "var(--c-player)"
            , Css.borderRadius Design.borderRadius.inner
            , Css.padding Design.spacing.xsmall
            , Css.color Design.color.white
            , Css.lineHeight (Css.int 1)
            , Css.fontSize Design.font.large
            ]
        ]
        [ H.text player.name ]


viewPlayer : Model -> Tichu.Game -> GamePlayer -> Html Msg
viewPlayer model game player =
    let
        hand =
            Cards.selectFrom (Cards.PlayerLocation Cards.Hand player.player) game.cards

        faceUp =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceUp) player.player) game.cards

        faceDown =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player.player) game.cards

        taken =
            Cards.selectFrom (Cards.PlayerLocation Cards.Taken player.player) game.cards

        style =
            { hand =
                [ Css.position Css.relative
                , Css.property "display" "grid"
                , Css.property "row-gap" Design.spacing.medium.value
                ]
            }
    in
    H.div
        []
        [ case game.phase of
            Tichu.PreGame state ->
                case Players.get player.player state of
                    Tichu.JustDealt ->
                        viewPlayerFront player.player ( faceUp, faceDown )

                    _ ->
                        H.text ""

            _ ->
                H.text ""
        , H.div
            [ css style.hand ]
            [ viewPlayerInfo game player
            , viewHand hand model.selectedCards
            ]
        ]


viewPlayerFront : Player -> ( List (Cards.Card Tichu.Suit), List (Cards.Card Tichu.Suit) ) -> Html Msg
viewPlayerFront player ( faceUp, faceDown ) =
    H.div
        []
        [ if List.length faceDown > 0 then
            H.div
                [ css
                    [ Css.position Css.relative
                    , Css.margin Design.spacing.medium
                    ]
                ]
                [ H.div
                    [ css
                        [ Css.position Css.absolute
                        , Css.top (pct 50)
                        , Css.left (pct 50)
                        , Css.transform (Css.translate2 (pct -50) (pct -50))
                        , Css.displayFlex
                        , Css.flexDirection Css.column
                        , Css.alignItems Css.center
                        ]
                    ]
                    [ Design.button.primary
                        "Pick up"
                        (Action (Tichu.PickUp player))
                        [ Css.width (pct 100)
                        ]
                    , H.p
                        [ css
                            [ Css.fontStyle Css.italic
                            , Css.margin2 Design.spacing.xsmall Css.zero
                            ]
                        ]
                        [ H.text "or" ]
                    , Design.button.primary
                        "Call Grand Tichu"
                        (Action (Tichu.CallGrandTichu player))
                        []
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


viewHand : List (Cards.Card Tichu.Suit) -> Set String -> Html Msg
viewHand hand selectedCards =
    viewCardList hand selectedCards


viewBet : Tichu.Bet -> Html Msg
viewBet bet =
    let
        betDisplay =
            \name ->
                H.p
                    [ css
                        [ Css.backgroundColor Design.color.lightestBlack
                        , Css.color Design.color.lightPrimary
                        , Css.padding Design.spacing.xsmall
                        , Css.marginLeft Design.spacing.large
                        , Css.paddingLeft Design.spacing.medium
                        , Css.borderRadius Design.borderRadius.inner
                        , Css.lineHeight (Css.num 1)
                        , Css.position Css.relative
                        ]
                    ]
                    [ H.span
                        [ css
                            [ Css.padding Design.spacing.xsmall
                            , Css.borderRadius (pct 100)
                            , Css.border3 (px 4) Css.solid Design.color.lightestBlack
                            , Css.lineHeight (Css.num 1)
                            , Css.backgroundColor Design.color.lightPrimary
                            , Css.color Design.color.lightestBlack
                            , Css.width (rem 2.5)
                            , Css.height (rem 2.5)
                            , Css.position Css.absolute
                            , Css.left (rem -1.5)
                            , Css.top (rem -0.25)
                            , Css.displayFlex
                            , Css.alignItems Css.center
                            , Css.justifyContent Css.center
                            , Css.fontWeight Css.bold
                            , Css.fontSize (Css.em 1.5)
                            ]
                        ]
                        [ H.text "!" ]
                    , H.text name
                    ]
    in
    case bet of
        Tichu.GrandTichu ->
            betDisplay "Grand Tichu"

        Tichu.Tichu ->
            betDisplay "Tichu"

        Tichu.None ->
            H.text ""


viewCardList : List (Cards.Card Tichu.Suit) -> Set String -> Html Msg
viewCardList cards selectedCards =
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
                            [ H.label
                                [ css
                                    [ Css.display Css.block
                                    , Css.Transitions.transition [ Css.Transitions.transform 150 ]
                                    , Css.transform
                                        (if Set.member c.id selectedCards then
                                            Css.translateY (px -20)

                                         else
                                            Css.translateY Css.zero
                                        )
                                    ]
                                ]
                                [ H.input
                                    [ A.type_ "checkbox"
                                    , A.checked (Set.member c.id selectedCards)
                                    , E.onCheck (ToggleCard c.id)
                                    , css
                                        [ Css.display Css.none
                                        ]
                                    ]
                                    []
                                , viewCard c
                                ]
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
                            (Css.stop <| Css.hex "dadada")
                            (Css.stop2 (Css.hex "dadada") <| pct 10)
                            [ Css.stop2 (Css.hex "ffd9d3") <| pct 10
                            , Css.stop2 (Css.hex "ffd9d3") <| pct 20
                            , Css.stop2 (Css.hex "d8f1d8") <| pct 20
                            , Css.stop2 (Css.hex "d8f1d8") <| pct 30
                            , Css.stop2 (Css.hex "cfdfff") <| pct 30
                            , Css.stop2 (Css.hex "cfdfff") <| pct 40
                            , Css.stop2 (Css.hex "fcf4db") <| pct 40
                            ]
                        )
                   ]
            )
        ]
        []


svgWithText : Svg msg -> String -> Html msg
svgWithText svg text =
    H.div
        [ css
            [ Css.displayFlex
            , Css.alignItems Css.center
            ]
        ]
        [ H.span
            [ css
                [ Css.width (rem 1.5)
                , Css.height (rem 1.5)
                , Css.marginRight Design.spacing.xsmall
                ]
            ]
            [ svg ]
        , H.span
            []
            [ H.text text ]
        ]


sharedStyle =
    let
        pxOverlap =
            25
    in
    { card =
        [ Css.width (px 100)
        , Css.height (px 150)
        , Css.border3 (px 1) Css.solid Design.color.black
        , Design.shadow.lowest
        , Css.borderRadius Design.borderRadius.inner
        , Css.backgroundColor Design.color.white
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
    , player =
        \player ->
            let
                playerString =
                    case player of
                        North ->
                            "north"

                        South ->
                            "south"

                        East ->
                            "east"

                        West ->
                            "west"
            in
            [ Css.property "--c-player" ("var(--c-" ++ playerString ++ ")")
            ]
    }
