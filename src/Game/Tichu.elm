module Game.Tichu exposing
    ( Action(..)
    , Bet(..)
    , Bomb(..)
    , Combination(..)
    , Game
    , PassChoices
    , Phase(..)
    , PlayerInfo
    , PlayingState(..)
    , PreGamePlayerInfo(..)
    , PreGameState(..)
    , Suit(..)
    , act
    , cardsInCombination
    , deckDefinition
    , determineCombination
    , getPlayersInfo
    , isPlayValid
    , newGame
    )

import Game.Cards as Cards exposing (Cards)
import Game.Players as Players exposing (Player(..), Players)
import List


type Suit
    = Special
    | Red
    | Green
    | Black
    | Blue


type alias Card =
    Cards.Card Suit


type Bomb
    = StraightFlush (List Card)
    | FourOfAKind (List Card)


type Combination
    = Single Card
    | Pair ( Card, Card )
    | Triple ( Card, Card, Card )
    | ConsecutivePairs (List Card)
    | Straight (List Card)
    | Bomb Bomb
    | Irregular (List Card)


type Action
    = PassCards Player
    | MarkForPass Player Card Player
    | TakeBackPass Player Card
    | PickUp Player
    | CallGrandTichu Player
    | CallTichu Player
    | Play Player Combination
    | Pass Player
    | TakeTrick Player
    | GiveDragon Player
    | Wish Int
    | EveryonePickUp


type Phase a b
    = PreGame a
    | Playing b


type PreGameState
    = JustDealt
    | PickedUp
    | PassedCards


type PlayingState
    = Lead
    | LeadAndWish
    | FulfillWish
    | WonTrick
    | WonTrickWithDragon
    | PlayOrPass
    | Idle
    | Passed


type Bet
    = None
    | Tichu
    | GrandTichu


type alias Game =
    { phase : Phase (Players PreGameState) (Players PlayingState)
    , cards : Cards Suit
    , bets : Players Bet
    }


newGame : Cards Suit -> Game
newGame cards =
    { phase = PreGame (Players.all JustDealt)
    , cards = cards
    , bets = Players.all None
    }


type alias PlayerInfo =
    { table : { self : Player, partner : Player, left : Player, right : Player }
    , bet : Bet
    , hand : List Card
    , cards : Phase PreGamePlayerInfo PlayingPlayerInfo
    }


type alias PassChoices =
    { partner : Maybe Card
    , left : Maybe Card
    , right : Maybe Card
    }


type PreGamePlayerInfo
    = LastSix (List Card)
    | SelectingPass PassChoices
    | DonePassing


type alias PlayingPlayerInfo =
    { taken : List Card
    , activePlays : List Int
    }


getPlayersInfo : Game -> Players PlayerInfo
getPlayersInfo game =
    Players.fromMap
        { north = getPlayerInfo North game
        , south = getPlayerInfo South game
        , east = getPlayerInfo East game
        , west = getPlayerInfo West game
        }


getPlayerInfo : Player -> Game -> PlayerInfo
getPlayerInfo player game =
    let
        table =
            { self = player
            , partner = Players.partner player
            , left = Players.left player
            , right = Players.right player
            }

        bet =
            Players.get player game.bets

        hand =
            Cards.selectFrom (Cards.PlayerLocation Cards.Hand player) game.cards

        faceDown =
            Cards.selectFrom (Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player) game.cards

        taken =
            Cards.selectFrom (Cards.PlayerLocation Cards.Taken player) game.cards

        cards =
            case game.phase of
                PreGame preGameState ->
                    PreGame
                        (case Players.get player preGameState of
                            JustDealt ->
                                LastSix faceDown

                            PickedUp ->
                                let
                                    cardPassed =
                                        \p ->
                                            List.head
                                                (Cards.selectFrom (Cards.PlayerLocation (Cards.PassingTo p) player) game.cards)
                                in
                                SelectingPass
                                    { partner = cardPassed table.partner
                                    , left = cardPassed table.left
                                    , right = cardPassed table.right
                                    }

                            PassedCards ->
                                DonePassing
                        )

                Playing playingState ->
                    Playing
                        { taken = taken
                        , activePlays = []
                        }
    in
    { table = table
    , bet = bet
    , hand = hand
    , cards = cards
    }


act : Action -> Game -> Game
act action game =
    let
        cardsAfterAction =
            Cards.act (cardAction action) game.cards
    in
    case game.phase of
        PreGame state ->
            case action of
                PickUp player ->
                    { game
                        | phase = PreGame (Players.set player PickedUp state)
                        , cards = cardsAfterAction
                    }

                MarkForPass _ _ _ ->
                    { game
                        | cards = cardsAfterAction
                    }

                TakeBackPass _ _ ->
                    { game
                        | cards = cardsAfterAction
                    }

                PassCards player ->
                    let
                        playersWhovePassed =
                            Players.filter
                                (\s -> s == PassedCards)
                                state
                    in
                    if List.length playersWhovePassed == 3 then
                        let
                            pickedUpCards =
                                Cards.act (cardAction EveryonePickUp) cardsAfterAction

                            birdLocation =
                                Cards.findById "bird" pickedUpCards

                            playerWithBird =
                                case birdLocation of
                                    Just card ->
                                        case card.location of
                                            Cards.PlayerLocation Cards.Hand p ->
                                                p

                                            _ ->
                                                North

                                    _ ->
                                        North

                            playingDefaultState =
                                Players.all Idle

                            playingState =
                                Players.set playerWithBird Lead playingDefaultState
                        in
                        { game
                            | phase = Playing playingState
                            , cards = pickedUpCards
                        }

                    else
                        { game
                            | phase = PreGame (Players.set player PassedCards state)
                            , cards = cardsAfterAction
                        }

                CallGrandTichu player ->
                    { game
                        | bets = Players.set player GrandTichu game.bets
                        , phase = PreGame (Players.set player PickedUp state)
                        , cards = cardsAfterAction
                    }

                CallTichu player ->
                    { game
                        | bets = Players.set player Tichu game.bets
                        , cards = cardsAfterAction
                    }

                _ ->
                    game

        Playing state ->
            case action of
                Play player _ ->
                    { game
                        | phase = Playing (Players.set player Idle state)
                        , cards = cardsAfterAction
                    }

                Pass player ->
                    let
                        playersPassed =
                            Players.filter
                                (\s -> s == Passed)
                                state
                    in
                    game

                CallTichu player ->
                    { game
                        | bets = Players.set player Tichu game.bets
                    }

                _ ->
                    game


deal : Cards.Deal
deal =
    [ ( Cards.PlayerLocation Cards.Hand North, 8 )
    , ( Cards.PlayerLocation Cards.Hand South, 8 )
    , ( Cards.PlayerLocation Cards.Hand East, 8 )
    , ( Cards.PlayerLocation Cards.Hand West, 8 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) North, 6 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) South, 6 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) East, 6 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) West, 6 )
    ]


deckDefinition : Cards.PlayableDeckDefinition Suit
deckDefinition =
    { deck = deck
    , deal = deal
    }


cardAction : Action -> Cards.ActionResult Suit
cardAction action =
    case action of
        PickUp player ->
            Cards.MoveCards
                (\card ->
                    card.location == Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player
                )
                (Cards.PlayerLocation Cards.Hand player)

        MarkForPass passer card passee ->
            Cards.MoveCards
                (\c -> c.definition.id == card.id)
                (Cards.PlayerLocation (Cards.PassingTo passee) passer)

        TakeBackPass passer card ->
            Cards.MoveCards
                (\c -> c.definition.id == card.id)
                (Cards.PlayerLocation Cards.Hand passer)

        PassCards passer ->
            Cards.MapCards
                (\card ->
                    case card.location of
                        Cards.PlayerLocation (Cards.PassingTo passee) maybeThisPasser ->
                            if maybeThisPasser == passer then
                                Cards.PlayerLocation (Cards.PassedTo passee) passer

                            else
                                card.location

                        _ ->
                            card.location
                )

        Play player combination ->
            Cards.PlayCards
                player
                (cardsInCombination combination)
                Cards.Table

        EveryonePickUp ->
            Cards.MapCards
                (\card ->
                    case card.location of
                        Cards.PlayerLocation (Cards.PassedTo passee) _ ->
                            Cards.PlayerLocation Cards.Hand passee

                        _ ->
                            card.location
                )

        CallTichu _ ->
            Cards.NoOp

        CallGrandTichu player ->
            Cards.MoveCards
                (\card ->
                    card.location == Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player
                )
                (Cards.PlayerLocation Cards.Hand player)

        Pass _ ->
            Cards.NoOp

        TakeTrick player ->
            Cards.MoveCards
                (\card ->
                    case card.location of
                        Cards.PlayedLocation Cards.Table _ ->
                            True

                        _ ->
                            False
                )
                (Cards.PlayerLocation Cards.Hand player)

        GiveDragon player ->
            Cards.MoveCards
                (\card ->
                    case card.location of
                        Cards.PlayedLocation Cards.Table _ ->
                            True

                        _ ->
                            False
                )
                (Cards.PlayerLocation Cards.Hand player)

        Wish _ ->
            Cards.NoOp


type PlayError
    = NotHigher
    | WrongCombination
    | WrongNumberOfCards
    | Invalid


isPlayValid : Combination -> Combination -> Result PlayError ()
isPlayValid first second =
    case ( first, second ) of
        ( Single firstCard, Single secondCard ) ->
            compareCombinations [ firstCard ] [ secondCard ]

        ( Pair ( firstPair, _ ), Pair ( secondPair, _ ) ) ->
            compareCombinations [ firstPair ] [ secondPair ]

        ( Triple ( firstTriple, _, _ ), Triple ( secondTriple, _, _ ) ) ->
            compareCombinations [ firstTriple ] [ secondTriple ]

        ( ConsecutivePairs firstCards, ConsecutivePairs secondCards ) ->
            compareCombinations firstCards secondCards

        ( Straight firstCards, Straight secondCards ) ->
            compareCombinations firstCards secondCards

        ( Bomb firstBomb, Bomb secondBomb ) ->
            case ( firstBomb, secondBomb ) of
                ( FourOfAKind firstFour, FourOfAKind secondFour ) ->
                    compareCombinations firstFour secondFour

                ( StraightFlush firstFlush, StraightFlush secondFlush ) ->
                    if List.length secondFlush > List.length firstFlush then
                        Ok ()

                    else
                        compareCombinations firstFlush secondFlush

                ( FourOfAKind _, StraightFlush _ ) ->
                    Ok ()

                ( StraightFlush _, FourOfAKind _ ) ->
                    Err NotHigher

        ( _, Bomb _ ) ->
            Ok ()

        ( _, Irregular _ ) ->
            Err Invalid

        _ ->
            Err WrongCombination


compareCombinations : List Card -> List Card -> Result PlayError ()
compareCombinations firstCards secondCards =
    if List.length secondCards /= List.length firstCards then
        Err WrongNumberOfCards

    else
        let
            firstLowest =
                List.head firstCards

            secondLowest =
                List.head secondCards
        in
        case ( firstLowest, secondLowest ) of
            ( Just firstStart, Just secondStart ) ->
                if firstStart.id == "phoenix" then
                    Ok ()

                else if secondStart.rank > firstStart.rank then
                    Ok ()

                else
                    Err NotHigher

            _ ->
                Err WrongCombination


cardsInCombination : Combination -> List Card
cardsInCombination combination =
    case combination of
        Single card ->
            [ card ]

        Pair ( c1, c2 ) ->
            [ c1, c2 ]

        Triple ( c1, c2, c3 ) ->
            [ c1, c2, c3 ]

        ConsecutivePairs cards ->
            cards

        Straight cards ->
            cards

        Bomb (FourOfAKind cards) ->
            cards

        Bomb (StraightFlush cards) ->
            cards

        Irregular cards ->
            cards


determineCombination : List Card -> Combination
determineCombination cards =
    let
        sortedCards =
            List.sortBy .rank cards

        maybeCombination =
            case sortedCards of
                [] ->
                    Nothing

                [ c ] ->
                    Maybe.map Single (parseSingle c)

                [ c1, c2 ] ->
                    Maybe.map Pair (parsePair c1 c2)

                [ c1, c2, c3 ] ->
                    Maybe.map Triple (parseTriple c1 c2 c3)

                [ c1, c2, c3, c4 ] ->
                    parseQuadruple c1 c2 c3 c4

                c :: cs ->
                    case parseStraight c cs of
                        Just combination ->
                            Just combination

                        Nothing ->
                            Maybe.map ConsecutivePairs (parseConsecutive (c :: cs))
    in
    Maybe.withDefault (Irregular cards) maybeCombination


parseSingle : Card -> Maybe Card
parseSingle card =
    if card.id == "dog" then
        Nothing

    else
        Just card


parsePair : Card -> Card -> Maybe ( Card, Card )
parsePair c1 c2 =
    if c1.rank == c2.rank then
        Just ( c1, c2 )

    else if c1.id == "phoenix" then
        Just ( c2, c1 )

    else if c2.id == "phoenix" then
        Just ( c1, c2 )

    else
        Nothing


parseTriple : Card -> Card -> Card -> Maybe ( Card, Card, Card )
parseTriple c1 c2 c3 =
    let
        formTriple =
            \( one, two ) ( _, three ) ->
                ( one, two, three )
    in
    Maybe.map2 formTriple (parsePair c1 c2) (parsePair c2 c3)


parseQuadruple : Card -> Card -> Card -> Card -> Maybe Combination
parseQuadruple c1 c2 c3 c4 =
    case parseFourCardBomb c1 c2 c3 c4 of
        Just cards ->
            Just (Bomb (FourOfAKind cards))

        Nothing ->
            Maybe.map ConsecutivePairs (parseConsecutive [ c1, c2, c3, c4 ])


parseFourCardBomb : Card -> Card -> Card -> Card -> Maybe (List Card)
parseFourCardBomb c1 c2 c3 c4 =
    let
        list =
            [ c1, c2, c3, c4 ]
    in
    if List.all (\c -> c.rank == c1.rank) list then
        Just list

    else
        Nothing


parseConsecutive : List Card -> Maybe (List Card)
parseConsecutive cards =
    if modBy 2 (List.length cards) /= 0 then
        Nothing

    else
        let
            containsPhoenix =
                not (List.isEmpty (List.filter (\c -> c.id == "phoenix") cards))

            validCards =
                List.isEmpty (List.filter (\c -> c.id == "dragon" || c.id == "dog") cards)
        in
        if not validCards then
            Nothing

        else
            case cards of
                first :: rest ->
                    let
                        ( isConsecutive, phoenixUsedAt ) =
                            testConsecutive first rest False containsPhoenix 0
                    in
                    if isConsecutive then
                        let
                            orderedWithPhoenix =
                                orderWithPhoenix phoenixUsedAt cards
                        in
                        Just orderedWithPhoenix

                    else
                        Nothing

                _ ->
                    Nothing


testConsecutive : Card -> List Card -> Bool -> Bool -> Int -> ( Bool, Int )
testConsecutive first rest foundTheOther canUsePhoenix depth =
    case rest of
        [] ->
            ( True, depth )

        x :: xs ->
            let
                usedPhoenixAt =
                    if canUsePhoenix then
                        depth + 1

                    else
                        depth
            in
            if x.id == "phoenix" then
                testConsecutive first xs False canUsePhoenix usedPhoenixAt

            else if first.id == "phoenix" then
                testConsecutive x xs False canUsePhoenix usedPhoenixAt

            else if x.rank == first.rank + 1 then
                if foundTheOther then
                    testConsecutive x xs False canUsePhoenix usedPhoenixAt

                else if canUsePhoenix then
                    testConsecutive x xs False False usedPhoenixAt

                else
                    ( False, depth )

            else if x.rank == first.rank then
                if not foundTheOther then
                    testConsecutive x xs True canUsePhoenix usedPhoenixAt

                else
                    ( False, depth )

            else
                ( False, depth )


parseStraight : Card -> List Card -> Maybe Combination
parseStraight first rest =
    let
        cards =
            first :: rest

        maybeBomb =
            List.all (\c -> c.suit == first.suit) rest

        containsPhoenix =
            not (List.isEmpty (List.filter (\c -> c.id == "phoenix") cards))

        validCards =
            List.isEmpty (List.filter (\c -> c.id == "dragon" || c.id == "dog") cards)
    in
    if not validCards then
        Nothing

    else
        let
            ( isStraight, phoenixUsedAt ) =
                testStraight first rest containsPhoenix 0
        in
        if isStraight && maybeBomb then
            Just (Bomb (StraightFlush cards))

        else if isStraight then
            let
                orderedWithPhoenix =
                    orderWithPhoenix phoenixUsedAt cards
            in
            Just (Straight orderedWithPhoenix)

        else
            Nothing


testStraight : Card -> List Card -> Bool -> Int -> ( Bool, Int )
testStraight first rest canUsePhoenix depth =
    case rest of
        [] ->
            ( True, depth )

        x :: xs ->
            let
                usedPhoenixAt =
                    if canUsePhoenix then
                        depth + 1

                    else
                        depth
            in
            if x.id == "phoenix" then
                if first.rank == 14 then
                    testStraight first xs False 0

                else
                    testStraight first xs canUsePhoenix usedPhoenixAt

            else if first.id == "phoenix" then
                testStraight x xs canUsePhoenix usedPhoenixAt

            else if x.rank == first.rank + 1 then
                testStraight x xs canUsePhoenix usedPhoenixAt

            else if x.rank == first.rank + 2 && canUsePhoenix then
                testStraight x xs False usedPhoenixAt

            else
                ( False, depth )


orderWithPhoenix : Int -> List Card -> List Card
orderWithPhoenix phoenixUsedAt cards =
    let
        phoenix =
            List.head (List.filter (\c -> c.id == "phoenix") cards)

        cardsWithoutPhoenix =
            List.filter (\c -> c.id /= "phoenix") cards

        orderedWithPhoenix =
            case phoenix of
                Just p ->
                    List.take phoenixUsedAt cardsWithoutPhoenix
                        ++ (p :: List.drop phoenixUsedAt cardsWithoutPhoenix)

                Nothing ->
                    cards
    in
    orderedWithPhoenix


deck : Cards.Deck Suit
deck =
    [ { id = "dragon"
      , displayName = "Dragon"
      , fullName = "Dragon"
      , value = 25
      , rank = 16
      , suit = Special
      }
    , { id = "dog"
      , displayName = "Dog"
      , fullName = "Dog"
      , value = 0
      , rank = 0
      , suit = Special
      }
    , { id = "phoenix"
      , displayName = "Phoenix"
      , fullName = "Phoenix"
      , value = -25
      , rank = 15
      , suit = Special
      }
    , { id = "bird"
      , displayName = "Bird"
      , fullName = "Bird"
      , value = 0
      , rank = 1
      , suit = Special
      }
    , { id = "green_2"
      , displayName = "2"
      , fullName = "Green 2"
      , value = 0
      , rank = 2
      , suit = Green
      }
    , { id = "green_3"
      , displayName = "3"
      , fullName = "Green 3"
      , value = 0
      , rank = 3
      , suit = Green
      }
    , { id = "green_4"
      , displayName = "4"
      , fullName = "Green 4"
      , value = 0
      , rank = 4
      , suit = Green
      }
    , { id = "green_5"
      , displayName = "5"
      , fullName = "Green 5"
      , value = 5
      , rank = 5
      , suit = Green
      }
    , { id = "green_6"
      , displayName = "6"
      , fullName = "Green 6"
      , value = 0
      , rank = 6
      , suit = Green
      }
    , { id = "green_7"
      , displayName = "7"
      , fullName = "Green 7"
      , value = 0
      , rank = 7
      , suit = Green
      }
    , { id = "green_8"
      , displayName = "8"
      , fullName = "Green 8"
      , value = 0
      , rank = 8
      , suit = Green
      }
    , { id = "green_9"
      , displayName = "9"
      , fullName = "Green 9"
      , value = 0
      , rank = 9
      , suit = Green
      }
    , { id = "green_10"
      , displayName = "10"
      , fullName = "Green 10"
      , value = 10
      , rank = 10
      , suit = Green
      }
    , { id = "green_J"
      , displayName = "J"
      , fullName = "Green Jack"
      , value = 0
      , rank = 11
      , suit = Green
      }
    , { id = "green_Q"
      , displayName = "Q"
      , fullName = "Green Queen"
      , value = 0
      , rank = 12
      , suit = Green
      }
    , { id = "green_K"
      , displayName = "K"
      , fullName = "Green King"
      , value = 10
      , rank = 13
      , suit = Green
      }
    , { id = "green_A"
      , displayName = "A"
      , fullName = "Green Ace"
      , value = 0
      , rank = 14
      , suit = Green
      }
    , { id = "black_2"
      , displayName = "2"
      , fullName = "Black 2"
      , value = 0
      , rank = 2
      , suit = Black
      }
    , { id = "black_3"
      , displayName = "3"
      , fullName = "Black 3"
      , value = 0
      , rank = 3
      , suit = Black
      }
    , { id = "black_4"
      , displayName = "4"
      , fullName = "Black 4"
      , value = 0
      , rank = 4
      , suit = Black
      }
    , { id = "black_5"
      , displayName = "5"
      , fullName = "Black 5"
      , value = 5
      , rank = 5
      , suit = Black
      }
    , { id = "black_6"
      , displayName = "6"
      , fullName = "Black 6"
      , value = 0
      , rank = 6
      , suit = Black
      }
    , { id = "black_7"
      , displayName = "7"
      , fullName = "Black 7"
      , value = 0
      , rank = 7
      , suit = Black
      }
    , { id = "black_8"
      , displayName = "8"
      , fullName = "Black 8"
      , value = 0
      , rank = 8
      , suit = Black
      }
    , { id = "black_9"
      , displayName = "9"
      , fullName = "Black 9"
      , value = 0
      , rank = 9
      , suit = Black
      }
    , { id = "black_10"
      , displayName = "10"
      , fullName = "Black 10"
      , value = 10
      , rank = 10
      , suit = Black
      }
    , { id = "black_J"
      , displayName = "J"
      , fullName = "Black Jack"
      , value = 0
      , rank = 11
      , suit = Black
      }
    , { id = "black_Q"
      , displayName = "Q"
      , fullName = "Black Queen"
      , value = 0
      , rank = 12
      , suit = Black
      }
    , { id = "black_K"
      , displayName = "K"
      , fullName = "Black King"
      , value = 10
      , rank = 13
      , suit = Black
      }
    , { id = "black_A"
      , displayName = "A"
      , fullName = "Black Ace"
      , value = 0
      , rank = 14
      , suit = Black
      }
    , { id = "blue_2"
      , displayName = "2"
      , fullName = "Blue 2"
      , value = 0
      , rank = 2
      , suit = Blue
      }
    , { id = "blue_3"
      , displayName = "3"
      , fullName = "Blue 3"
      , value = 0
      , rank = 3
      , suit = Blue
      }
    , { id = "blue_4"
      , displayName = "4"
      , fullName = "Blue 4"
      , value = 0
      , rank = 4
      , suit = Blue
      }
    , { id = "blue_5"
      , displayName = "5"
      , fullName = "Blue 5"
      , value = 5
      , rank = 5
      , suit = Blue
      }
    , { id = "blue_6"
      , displayName = "6"
      , fullName = "Blue 6"
      , value = 0
      , rank = 6
      , suit = Blue
      }
    , { id = "blue_7"
      , displayName = "7"
      , fullName = "Blue 7"
      , value = 0
      , rank = 7
      , suit = Blue
      }
    , { id = "blue_8"
      , displayName = "8"
      , fullName = "Blue 8"
      , value = 0
      , rank = 8
      , suit = Blue
      }
    , { id = "blue_9"
      , displayName = "9"
      , fullName = "Blue 9"
      , value = 0
      , rank = 9
      , suit = Blue
      }
    , { id = "blue_10"
      , displayName = "10"
      , fullName = "Blue 10"
      , value = 10
      , rank = 10
      , suit = Blue
      }
    , { id = "blue_J"
      , displayName = "J"
      , fullName = "Blue Jack"
      , value = 0
      , rank = 11
      , suit = Blue
      }
    , { id = "blue_Q"
      , displayName = "Q"
      , fullName = "Blue Queen"
      , value = 0
      , rank = 12
      , suit = Blue
      }
    , { id = "blue_K"
      , displayName = "K"
      , fullName = "Blue King"
      , value = 10
      , rank = 13
      , suit = Blue
      }
    , { id = "blue_A"
      , displayName = "A"
      , fullName = "Blue Ace"
      , value = 0
      , rank = 14
      , suit = Blue
      }
    , { id = "red_2"
      , displayName = "2"
      , fullName = "Red 2"
      , value = 0
      , rank = 2
      , suit = Red
      }
    , { id = "red_3"
      , displayName = "3"
      , fullName = "Red 3"
      , value = 0
      , rank = 3
      , suit = Red
      }
    , { id = "red_4"
      , displayName = "4"
      , fullName = "Red 4"
      , value = 0
      , rank = 4
      , suit = Red
      }
    , { id = "red_5"
      , displayName = "5"
      , fullName = "Red 5"
      , value = 5
      , rank = 5
      , suit = Red
      }
    , { id = "red_6"
      , displayName = "6"
      , fullName = "Red 6"
      , value = 0
      , rank = 6
      , suit = Red
      }
    , { id = "red_7"
      , displayName = "7"
      , fullName = "Red 7"
      , value = 0
      , rank = 7
      , suit = Red
      }
    , { id = "red_8"
      , displayName = "8"
      , fullName = "Red 8"
      , value = 0
      , rank = 8
      , suit = Red
      }
    , { id = "red_9"
      , displayName = "9"
      , fullName = "Red 9"
      , value = 0
      , rank = 9
      , suit = Red
      }
    , { id = "red_10"
      , displayName = "10"
      , fullName = "Red 10"
      , value = 10
      , rank = 10
      , suit = Red
      }
    , { id = "red_J"
      , displayName = "J"
      , fullName = "Red Jack"
      , value = 0
      , rank = 11
      , suit = Red
      }
    , { id = "red_Q"
      , displayName = "Q"
      , fullName = "Red Queen"
      , value = 0
      , rank = 12
      , suit = Red
      }
    , { id = "red_K"
      , displayName = "K"
      , fullName = "Red King"
      , value = 10
      , rank = 13
      , suit = Red
      }
    , { id = "red_A"
      , displayName = "A"
      , fullName = "Red Ace"
      , value = 0
      , rank = 14
      , suit = Red
      }
    ]
