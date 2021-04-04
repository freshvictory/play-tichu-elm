module Page.Game exposing (Model, Msg, init, update, view)

import Css exposing (pct, px, rem)
import Css.Global
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
    , activePlayer : Maybe Player
    , currentPlayers : Players GamePlayer
    , selectedCards : Players (Set String)
    , states : List (Game Tichu.Suit)
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
      , activePlayer = Just North
      , currentPlayers = currentPlayers
      , selectedCards = Players.all Set.empty
      , states = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = NoOp
    | Shuffle
    | DeckShuffled (Cards.Deck Tichu.Suit)
    | Action Tichu.Action
    | ToggleCard Player String Bool
    | Rewind
    | ChangeActivePlayer String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle ->
            ( model, shuffleDeck model.deck.deck )

        DeckShuffled cards ->
            ( { model
                | game = Dealt (Tichu.newGame (model.deck.deal cards))
                , selectedCards = Players.all Set.empty
              }
            , Cmd.none
            )

        Action action ->
            case model.game of
                Undealt ->
                    ( model, Cmd.none )

                Dealt game ->
                    let
                        newGame =
                            Dealt (Tichu.act action game)

                        newSelected =
                            case action of
                                Tichu.MarkForPass player card _ ->
                                    Players.set player (Set.remove card.id (Players.get player model.selectedCards)) model.selectedCards

                                Tichu.Play player _ _ ->
                                    Players.set player Set.empty model.selectedCards

                                _ ->
                                    model.selectedCards
                    in
                    ( { model
                        | game = newGame
                        , selectedCards = newSelected
                        , states = Dealt game :: model.states
                      }
                    , Cmd.none
                    )

        ToggleCard player id checked ->
            ( { model
                | selectedCards =
                    Players.set player
                        (if checked then
                            Set.insert id (Players.get player model.selectedCards)

                         else
                            Set.remove id (Players.get player model.selectedCards)
                        )
                        model.selectedCards
              }
            , Cmd.none
            )

        Rewind ->
            case model.states of
                [] ->
                    ( model, Cmd.none )

                state :: rest ->
                    ( { model
                        | game = state
                        , states = rest
                      }
                    , Cmd.none
                    )

        ChangeActivePlayer player ->
            ( { model
                | activePlayer =
                    case player of
                        "Lyle" ->
                            Just North

                        "Lydia" ->
                            Just South

                        "Justin" ->
                            Just East

                        "Nick" ->
                            Just West

                        _ ->
                            Nothing
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
                viewTable model game

            Undealt ->
                H.text ""
        ]


viewGameHeader : Model -> Html Msg
viewGameHeader model =
    H.header
        [ css
            [ Css.margin Design.spacing.medium
            , Css.property "display" "grid"
            , Css.property "grid-template-columns" "1fr auto"
            , Css.property "grid-auto-flow" "column"
            , Css.property "column-gap" Design.spacing.xsmall.value
            , Css.alignItems Css.center
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
                , Css.marginRight Css.auto
                ]
            ]
            [ H.text "tichu" ]
        , H.select
            [ E.onInput ChangeActivePlayer ]
          <|
            List.map
                (\player ->
                    H.option
                        [ A.selected (model.activePlayer == Just player)
                        ]
                        [ H.text (Players.get player model.currentPlayers).name ]
                )
                [ North, South, East, West ]
        , Design.button.secondary "Rewind" Rewind []
        , Design.button.secondary "Shuffle" Shuffle []
        ]


viewTable : Model -> Tichu.Game -> Html Msg
viewTable model game =
    let
        playersInfo =
            Tichu.getPlayersInfo game

        currentPlayerInfo =
            Maybe.map (\p -> Players.get p playersInfo) model.activePlayer

        order =
            case model.activePlayer of
                Just p ->
                    { top = Players.partner p
                    , left = Players.left p
                    , bottom = p
                    , right = Players.right p
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
            [ viewPlayerInfo model (Players.get order.top playersInfo) currentPlayerInfo 0 ]
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
                [ viewPlayerInfo model (Players.get order.left playersInfo) currentPlayerInfo -90 ]
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
                [ viewPlayerInfo model (Players.get order.right playersInfo) currentPlayerInfo 90 ]
            ]
        , H.div
            [ css
                [ Css.property "grid-area" "table"
                , Css.padding Design.spacing.small
                ]
            ]
            [ case model.activePlayer of
                Just current ->
                    let
                        info =
                            Players.get current playersInfo
                    in
                    case info.cards of
                        Tichu.PreGame (Tichu.SelectingPass cardsPassed) ->
                            if cardsPassed.left /= Nothing && cardsPassed.right /= Nothing && cardsPassed.partner /= Nothing then
                                Design.button.primary
                                    "Confirm pass"
                                    (Action (Tichu.PassCards current))
                                    [ css
                                        [ Css.position Css.absolute
                                        , Css.left (pct 50)
                                        , Css.top (pct 50)
                                        , Css.transforms
                                            [ Css.translate2 (pct -50) (pct -50)
                                            ]
                                        ]
                                    ]

                            else
                                H.text ""

                        _ ->
                            case game.phase of
                                Tichu.PreGame _ ->
                                    H.text ""

                                Tichu.Playing _ ->
                                    viewCardsInPlay model game

                Nothing ->
                    case game.phase of
                        Tichu.PreGame _ ->
                            H.text ""

                        Tichu.Playing _ ->
                            viewCardsInPlay model game
            ]
        , H.div
            [ css
                [ Css.property "grid-area" "me" ]
            ]
            [ case model.activePlayer of
                Just p ->
                    viewPlayer model (Players.get p playersInfo)

                Nothing ->
                    viewPlayerInfo model (Players.get order.bottom playersInfo) Nothing 0
            ]
        ]


viewCardsInPlay : Model -> Tichu.Game -> Html Msg
viewCardsInPlay model game =
    let
        cardsOnTable =
            Cards.byPlay Cards.Table game.cards

        combinations =
            List.map
                (\( player, cardsInPlay ) ->
                    let
                        ( combination, cards ) =
                            Tichu.determineCombination cardsInPlay
                    in
                    ( player, combination, cards )
                )
                cardsOnTable

        firstCombination =
            case combinations of
                [] ->
                    Nothing

                ( _, Tichu.Dog, _ ) :: cs ->
                    case cs of
                        [] ->
                            Nothing

                        ( _, combination, cards ) :: _ ->
                            Just ( combination, cards )

                ( _, combination, cards ) :: _ ->
                    Just ( combination, cards )
    in
    H.section
        []
        [ H.header
            [ css
                [ Css.marginBottom Design.spacing.small
                ]
            ]
            [ viewCurrentPlay model game firstCombination
            ]
        , H.ol
            [ css
                [ Css.displayFlex
                , Css.flexWrap Css.wrap
                ]
            ]
          <|
            List.map
                (\( player, combination, cards ) ->
                    H.li
                        [ css
                            [ Css.marginRight Design.spacing.large
                            , Css.marginBottom Design.spacing.large
                            ]
                        ]
                        [ viewCombination ( combination, cards ) (Players.get player model.currentPlayers) ]
                )
                combinations
        ]


viewCurrentPlay : Model -> Tichu.Game -> Maybe ( Tichu.Combination, List (Cards.Card Tichu.Suit) ) -> Html Msg
viewCurrentPlay model game combination =
    H.div
        []
        [ case combination of
            Just c ->
                H.h2
                    [ css
                        [ Css.fontSize Design.font.large
                        , Css.textAlign Css.center
                        ]
                    ]
                    [ H.text (Tichu.describeCombination c) ]

            _ ->
                H.text ""
        ]


viewCombination : ( Tichu.Combination, List (Cards.Card Tichu.Suit) ) -> GamePlayer -> Html Msg
viewCombination ( combination, cards ) player =
    H.article
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "max-content auto"
            , Css.property "grid-column-gap" Design.spacing.small.value
            , Css.maxWidth Css.maxContent
            , sharedStyle.player player.player
            , Css.property "background-color" Design.color.lightTable.value
            , Css.borderRadius Design.borderRadius.outer
            , Css.padding Design.spacing.small
            , Design.shadow.lowest
            ]
        ]
        [ H.header
            [ css
                [ Css.color Design.color.white
                ]
            ]
            [ viewPlayerTag player
            ]
        , H.ol [ css (sharedStyle.cardList ++ sharedStyle.cardListCollapsed) ] <|
            List.map
                (\card ->
                    H.li
                        [ css sharedStyle.cardListItem ]
                        [ viewCard card ]
                )
                cards
        ]


viewPlayerInfo : Model -> Tichu.PlayerInfo -> Maybe Tichu.PlayerInfo -> Float -> Html Msg
viewPlayerInfo model info currentPlayer rotation =
    H.div
        [ css
            [ Css.property "display" "grid"
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
            , Css.position Css.relative
            ]
        ]
        [ viewPlayerTag (Players.get info.table.self model.currentPlayers)
        , H.span
            [ css
                [ Svg.primaryColor Design.color.lightPrimary.value
                ]
            ]
            [ svgWithText Svg.hand (String.fromInt (List.length info.hand)) ]
        , case info.cards of
            Tichu.PreGame _ ->
                H.text ""

            Tichu.Playing state ->
                H.span
                    [ css
                        [ Svg.primaryColor Design.color.lightPrimary.value
                        ]
                    ]
                    [ svgWithText Svg.stack (String.fromInt (List.length state.taken)) ]
        , viewBet info.bet
        , case currentPlayer of
            Just current ->
                case current.cards of
                    Tichu.PreGame (Tichu.SelectingPass cardsPassed) ->
                        let
                            cardPassed =
                                if current.table.left == info.table.self then
                                    cardsPassed.left

                                else if current.table.partner == info.table.self then
                                    cardsPassed.partner

                                else
                                    cardsPassed.right
                        in
                        case cardPassed of
                            Just card ->
                                viewPassedCard current.table.self card rotation

                            Nothing ->
                                H.text ""

                    _ ->
                        H.text ""

            Nothing ->
                H.text ""
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
            , sharedStyle.player player.player
            ]
        ]
        [ H.text player.name ]


viewPlayer : Model -> Tichu.PlayerInfo -> Html Msg
viewPlayer model info =
    let
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
        [ case info.cards of
            Tichu.PreGame (Tichu.LastSix cards) ->
                viewPlayerFront info.table.self ( [], cards )

            _ ->
                H.text ""
        , H.div
            [ css style.hand ]
            [ viewPlayerInfo model info Nothing 0
            , case info.cards of
                Tichu.Playing _ ->
                    let
                        selectedCardIds =
                            Players.get info.table.self model.selectedCards

                        selectedCards =
                            List.filter (\c -> Set.member c.id selectedCardIds) info.hand

                        ( combination, cardsInCombination ) =
                            Tichu.determineCombination selectedCards
                    in
                    if not (Set.isEmpty selectedCardIds) then
                        H.div
                            [ css
                                [ Css.margin Css.auto
                                , Css.displayFlex
                                , Css.flexDirection Css.column
                                ]
                            ]
                            [ Design.button.primary
                                ("Play " ++ Tichu.describeCombination ( combination, cardsInCombination ))
                                (Action (Tichu.Play info.table.self combination cardsInCombination))
                                []
                            ]

                    else
                        H.text ""

                _ ->
                    H.text ""
            , viewHand model info viewSelectedCard
            ]
        ]


viewSelectedCard : Model -> Tichu.PlayerInfo -> Cards.Card Tichu.Suit -> Html Msg
viewSelectedCard model info card =
    case info.cards of
        Tichu.PreGame state ->
            case state of
                Tichu.SelectingPass cards ->
                    viewPassDialog model info cards card

                _ ->
                    H.text ""

        _ ->
            H.text ""


viewPassDialog : Model -> Tichu.PlayerInfo -> Tichu.PassChoices -> Cards.Card Tichu.Suit -> Html Msg
viewPassDialog model info cardsPassed card =
    let
        playerButton =
            \p cardPassed label ->
                let
                    name =
                        (Players.get p model.currentPlayers).name
                in
                H.li
                    [ css
                        [ sharedStyle.player p
                        , Css.property "grid-area" label
                        , Css.margin2 Design.spacing.small Css.zero
                        , Css.lastChild [ Css.marginBottom Css.zero ]
                        ]
                    ]
                    [ Design.button.custom
                        name
                        (Action (Tichu.MarkForPass info.table.self card p))
                        [ css
                            [ Css.property "background-color" "var(--c-player)"
                            , Css.color Design.color.white
                            , Css.width (pct 100)
                            , Css.disabled
                                [ Css.opacity (Css.num 0.5)
                                ]
                            , Css.active
                                [ Css.backgroundColor Design.color.gray
                                ]
                            ]
                        , A.disabled (cardPassed /= Nothing)
                        ]
                    ]
    in
    H.div
        [ css
            [ Css.position Css.absolute
            , Css.bottom (Css.calc (pct 100) Css.plus Design.spacing.xlarge)
            , Css.left (pct 50)
            , Css.transform (Css.translateX (pct -50))
            , Css.backgroundColor Design.color.lightestTable
            , Css.borderRadius Design.borderRadius.middle
            , Css.padding Design.spacing.small
            , Css.minWidth (px 175)
            , Design.shadow.middle
            ]
        ]
        [ H.h2
            [ css
                [ Css.fontSize Design.font.large
                , Css.textAlign Css.center
                , Css.fontStyle Css.italic
                , Css.marginBottom Design.spacing.xsmall
                , Css.paddingBottom Design.spacing.xxsmall
                , Css.fontWeight Css.normal
                , Css.borderBottom3 (px 1) Css.solid Design.color.darkGray
                ]
            ]
            [ H.text "Pass this card" ]
        , H.ol
            [ css
                []
            ]
            [ playerButton info.table.partner cardsPassed.partner "Partner"
            , playerButton info.table.left cardsPassed.left "Left"
            , playerButton info.table.right cardsPassed.right "Right"
            ]
        ]


viewPassedCard : Player -> Cards.Card Tichu.Suit -> Float -> Html Msg
viewPassedCard player card rotation =
    H.div
        [ css
            [ Css.position Css.absolute
            , Css.left (pct 50)
            , Css.transforms
                [ Css.translateX (pct -50)
                , Css.rotateZ (Css.deg -rotation)
                ]
            , Css.top (Css.calc (pct 100) Css.plus Design.spacing.medium)
            ]
        ]
        [ viewCard card
        , Design.button.secondary
            "Take Back"
            (Action (Tichu.TakeBackPass player card))
            [ css
                [ Css.position Css.absolute
                , Css.left (pct 50)
                , Css.top (Css.calc (pct 100) Css.plus Design.spacing.small)
                , Css.transforms
                    [ Css.translateX (pct -50)
                    ]
                ]
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
                        [ css [ Css.width (pct 100) ]
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


viewHand : Model -> Tichu.PlayerInfo -> (Model -> Tichu.PlayerInfo -> Cards.Card Tichu.Suit -> Html Msg) -> Html Msg
viewHand model info viewSelected =
    viewCardList model info info.hand viewSelected


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


viewCardList : Model -> Tichu.PlayerInfo -> List (Cards.Card Tichu.Suit) -> (Model -> Tichu.PlayerInfo -> Cards.Card Tichu.Suit -> Html Msg) -> Html Msg
viewCardList model info cards viewSelected =
    let
        sortedCards =
            List.sortBy (\c -> c.rank) cards

        selectedCards =
            Players.get info.table.self model.selectedCards
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
                            [ css
                                (sharedStyle.cardListItem
                                    ++ [ Css.position Css.relative ]
                                )
                            ]
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
                                    , E.onCheck (ToggleCard info.table.self c.id)
                                    , css
                                        [ Css.display Css.none
                                        ]
                                    ]
                                    []
                                , viewCard c
                                ]
                            , if Set.member c.id selectedCards then
                                viewSelected model info c

                              else
                                H.text ""
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
                ++ [ Css.hover
                        [ Design.shadow.hover
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
    , cardListCollapsed =
        [ Css.Global.children
            [ Css.Global.li
                [ Css.marginLeft (px -70)
                , Css.firstChild
                    [ Css.marginLeft (px -pxOverlap) ]
                ]
            ]
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
            Css.property "--c-player" ("var(--c-" ++ playerString ++ ")")
    }
