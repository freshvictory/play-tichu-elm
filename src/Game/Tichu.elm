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
    , cardDefinition
    , deckDefinition
    , describeCombination
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
    = StraightFlush
    | FourOfAKind


type Combination
    = Single
    | Pair
    | Triple
    | FullHouse
    | ConsecutivePairs
    | Straight
    | Bomb Bomb
    | Dog
    | Irregular


type Action
    = PassCards Player
    | MarkForPass Player Card Player
    | TakeBackPass Player Card
    | PickUp Player
    | CallGrandTichu Player
    | CallTichu Player
    | Play Player Combination (List Card)
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
                Play player _ _ ->
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

        Play player _ cards ->
            Cards.PlayCards
                player
                cards
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


isPlayValid : ( Combination, List Card ) -> ( Combination, List Card ) -> Result PlayError ()
isPlayValid ( first, firstCards ) ( second, secondCards ) =
    if first == Bomb StraightFlush && second == Bomb StraightFlush then
        if List.length secondCards > List.length firstCards then
            Ok ()

        else
            compareCombinations firstCards secondCards

    else if first == second then
        compareCombinations firstCards secondCards

    else if first == Bomb FourOfAKind && second == Bomb StraightFlush then
        Ok ()

    else if second == Bomb StraightFlush then
        Ok ()

    else if first == Dog then
        Ok ()

    else
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


determineCombination : List Card -> ( Combination, List Card )
determineCombination cards =
    let
        sortedCards =
            List.sortBy .rank cards

        ( maybeCombination, cardsInCombination ) =
            case sortedCards of
                [] ->
                    ( Nothing, cards )

                [ c ] ->
                    parseSingle c

                [ c1, c2 ] ->
                    parsePair c1 c2

                [ c1, c2, c3 ] ->
                    parseTriple c1 c2 c3

                [ c1, c2, c3, c4 ] ->
                    parseQuadruple c1 c2 c3 c4

                [ c1, c2, c3, c4, c5 ] ->
                    case parseStraight c1 [ c2, c3, c4, c5 ] of
                        ( Just combination, straightCards ) ->
                            ( Just combination, straightCards )

                        ( Nothing, _ ) ->
                            parseFullHouse c1 c2 c3 c4 c5

                c :: cs ->
                    case parseStraight c cs of
                        ( Just combination, straightCards ) ->
                            ( Just combination, straightCards )

                        ( Nothing, _ ) ->
                            parseConsecutive (c :: cs)
    in
    ( Maybe.withDefault Irregular maybeCombination, cardsInCombination )


parseSingle : Card -> ( Maybe Combination, List Card )
parseSingle card =
    if card == cardDefinition.dog then
        ( Just Dog, [ card ] )

    else
        ( Just Single, [ card ] )


parsePair : Card -> Card -> ( Maybe Combination, List Card )
parsePair c1 c2 =
    if c1 == cardDefinition.dragon || c2 == cardDefinition.dragon then
        ( Nothing, [ c1, c2 ] )

    else if c1 == cardDefinition.dog || c2 == cardDefinition.dog then
        ( Nothing, [ c1, c2 ] )

    else if c1.rank == c2.rank then
        ( Just Pair, [ c1, c2 ] )

    else if c1 == cardDefinition.phoenix then
        ( Just Pair, [ c2, c1 ] )

    else if c2 == cardDefinition.phoenix then
        ( Just Pair, [ c1, c2 ] )

    else
        ( Nothing, [ c1, c2 ] )


parseTriple : Card -> Card -> Card -> ( Maybe Combination, List Card )
parseTriple c1 c2 c3 =
    if c1.rank == c2.rank && c2.rank == c3.rank then
        ( Just Triple, [ c1, c2, c3 ] )

    else if c1.rank == c2.rank && c3 == cardDefinition.phoenix then
        ( Just Triple, [ c1, c2, c3 ] )

    else if c1.rank == c3.rank && c2 == cardDefinition.phoenix then
        ( Just Triple, [ c1, c3, c2 ] )

    else if c2.rank == c3.rank && c1 == cardDefinition.phoenix then
        ( Just Triple, [ c2, c3, c1 ] )

    else
        ( Nothing, [ c1, c2, c3 ] )


parseFullHouse : Card -> Card -> Card -> Card -> Card -> ( Maybe Combination, List Card )
parseFullHouse c1 c2 c3 c4 c5 =
    case parseFourCardBomb c1 c2 c3 c4 of
        Just _ ->
            ( Nothing, [ c1, c2, c3, c4, c5 ] )

        _ ->
            Maybe.withDefault ( Nothing, [ c1, c2, c3, c4, c5 ] ) (checkFullHouse c1 c2 c3 c4 c5 3)


checkFullHouse : Card -> Card -> Card -> Card -> Card -> Int -> Maybe ( Maybe Combination, List Card )
checkFullHouse c1 c2 c3 c4 c5 count =
    if count == 0 then
        Nothing

    else
        case ( parseTriple c1 c2 c3, parsePair c4 c5 ) of
            ( ( Just _, triple ), ( Just _, pair ) ) ->
                Just ( Just FullHouse, triple ++ pair )

            _ ->
                checkFullHouse c2 c3 c4 c5 c1 (count - 1)


parseQuadruple : Card -> Card -> Card -> Card -> ( Maybe Combination, List Card )
parseQuadruple c1 c2 c3 c4 =
    case parseFourCardBomb c1 c2 c3 c4 of
        Just _ ->
            ( Just (Bomb FourOfAKind), [ c1, c2, c3, c4 ] )

        Nothing ->
            parseConsecutive [ c1, c2, c3, c4 ]


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


parseConsecutive : List Card -> ( Maybe Combination, List Card )
parseConsecutive cards =
    if modBy 2 (List.length cards) /= 0 then
        ( Nothing, cards )

    else
        let
            containsPhoenix =
                not (List.isEmpty (List.filter (\c -> c.id == "phoenix") cards))

            validCards =
                List.isEmpty (List.filter (\c -> c.id == "dragon" || c.id == "dog") cards)
        in
        if not validCards then
            ( Nothing, cards )

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
                        ( Just ConsecutivePairs, orderedWithPhoenix )

                    else
                        ( Nothing, cards )

                _ ->
                    ( Nothing, cards )


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


parseStraight : Card -> List Card -> ( Maybe Combination, List Card )
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
        ( Nothing, cards )

    else
        let
            ( isStraight, phoenixUsedAt ) =
                testStraight first rest containsPhoenix 0
        in
        if isStraight && maybeBomb then
            ( Just (Bomb StraightFlush), cards )

        else if isStraight then
            let
                orderedWithPhoenix =
                    orderWithPhoenix phoenixUsedAt cards
            in
            ( Just Straight, orderedWithPhoenix )

        else
            ( Nothing, cards )


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


describeCombination : ( Combination, List Card ) -> String
describeCombination ( combination, cards ) =
    case combination of
        Single ->
            "Single"

        Pair ->
            "Pair"

        Triple ->
            "Triple"

        FullHouse ->
            "Full House"

        ConsecutivePairs ->
            String.fromInt (List.length cards // 2) ++ " Consecutive Pairs"

        Straight ->
            String.fromInt (List.length cards) ++ "-Card Straight"

        Bomb StraightFlush ->
            String.fromInt (List.length cards) ++ "-Card Straight Flush Bomb"

        Bomb FourOfAKind ->
            "Bomb"

        Dog ->
            "The Dog"

        Irregular ->
            "Unknown"


deck : Cards.Deck Suit
deck =
    [ cardDefinition.dragon
    , cardDefinition.dog
    , cardDefinition.phoenix
    , cardDefinition.bird
    , cardDefinition.green_2
    , cardDefinition.green_3
    , cardDefinition.green_4
    , cardDefinition.green_5
    , cardDefinition.green_6
    , cardDefinition.green_7
    , cardDefinition.green_8
    , cardDefinition.green_9
    , cardDefinition.green_10
    , cardDefinition.green_J
    , cardDefinition.green_Q
    , cardDefinition.green_K
    , cardDefinition.green_A
    , cardDefinition.black_2
    , cardDefinition.black_3
    , cardDefinition.black_4
    , cardDefinition.black_5
    , cardDefinition.black_6
    , cardDefinition.black_7
    , cardDefinition.black_8
    , cardDefinition.black_9
    , cardDefinition.black_10
    , cardDefinition.black_J
    , cardDefinition.black_Q
    , cardDefinition.black_K
    , cardDefinition.black_A
    , cardDefinition.blue_2
    , cardDefinition.blue_3
    , cardDefinition.blue_4
    , cardDefinition.blue_5
    , cardDefinition.blue_6
    , cardDefinition.blue_7
    , cardDefinition.blue_8
    , cardDefinition.blue_9
    , cardDefinition.blue_10
    , cardDefinition.blue_J
    , cardDefinition.blue_Q
    , cardDefinition.blue_K
    , cardDefinition.blue_A
    , cardDefinition.red_2
    , cardDefinition.red_3
    , cardDefinition.red_4
    , cardDefinition.red_5
    , cardDefinition.red_6
    , cardDefinition.red_7
    , cardDefinition.red_8
    , cardDefinition.red_9
    , cardDefinition.red_10
    , cardDefinition.red_J
    , cardDefinition.red_Q
    , cardDefinition.red_K
    , cardDefinition.red_A
    ]


cardDefinition =
    { dragon =
        { id = "dragon"
        , displayName = "Dragon"
        , fullName = "Dragon"
        , value = 25
        , rank = 16
        , suit = Special
        }
    , dog =
        { id = "dog"
        , displayName = "Dog"
        , fullName = "Dog"
        , value = 0
        , rank = 0
        , suit = Special
        }
    , phoenix =
        { id = "phoenix"
        , displayName = "Phoenix"
        , fullName = "Phoenix"
        , value = -25
        , rank = 15
        , suit = Special
        }
    , bird =
        { id = "bird"
        , displayName = "Bird"
        , fullName = "Bird"
        , value = 0
        , rank = 1
        , suit = Special
        }
    , green_2 =
        { id = "green_2"
        , displayName = "2"
        , fullName = "Green 2"
        , value = 0
        , rank = 2
        , suit = Green
        }
    , green_3 =
        { id = "green_3"
        , displayName = "3"
        , fullName = "Green 3"
        , value = 0
        , rank = 3
        , suit = Green
        }
    , green_4 =
        { id = "green_4"
        , displayName = "4"
        , fullName = "Green 4"
        , value = 0
        , rank = 4
        , suit = Green
        }
    , green_5 =
        { id = "green_5"
        , displayName = "5"
        , fullName = "Green 5"
        , value = 5
        , rank = 5
        , suit = Green
        }
    , green_6 =
        { id = "green_6"
        , displayName = "6"
        , fullName = "Green 6"
        , value = 0
        , rank = 6
        , suit = Green
        }
    , green_7 =
        { id = "green_7"
        , displayName = "7"
        , fullName = "Green 7"
        , value = 0
        , rank = 7
        , suit = Green
        }
    , green_8 =
        { id = "green_8"
        , displayName = "8"
        , fullName = "Green 8"
        , value = 0
        , rank = 8
        , suit = Green
        }
    , green_9 =
        { id = "green_9"
        , displayName = "9"
        , fullName = "Green 9"
        , value = 0
        , rank = 9
        , suit = Green
        }
    , green_10 =
        { id = "green_10"
        , displayName = "10"
        , fullName = "Green 10"
        , value = 10
        , rank = 10
        , suit = Green
        }
    , green_J =
        { id = "green_J"
        , displayName = "J"
        , fullName = "Green Jack"
        , value = 0
        , rank = 11
        , suit = Green
        }
    , green_Q =
        { id = "green_Q"
        , displayName = "Q"
        , fullName = "Green Queen"
        , value = 0
        , rank = 12
        , suit = Green
        }
    , green_K =
        { id = "green_K"
        , displayName = "K"
        , fullName = "Green King"
        , value = 10
        , rank = 13
        , suit = Green
        }
    , green_A =
        { id = "green_A"
        , displayName = "A"
        , fullName = "Green Ace"
        , value = 0
        , rank = 14
        , suit = Green
        }
    , black_2 =
        { id = "black_2"
        , displayName = "2"
        , fullName = "Black 2"
        , value = 0
        , rank = 2
        , suit = Black
        }
    , black_3 =
        { id = "black_3"
        , displayName = "3"
        , fullName = "Black 3"
        , value = 0
        , rank = 3
        , suit = Black
        }
    , black_4 =
        { id = "black_4"
        , displayName = "4"
        , fullName = "Black 4"
        , value = 0
        , rank = 4
        , suit = Black
        }
    , black_5 =
        { id = "black_5"
        , displayName = "5"
        , fullName = "Black 5"
        , value = 5
        , rank = 5
        , suit = Black
        }
    , black_6 =
        { id = "black_6"
        , displayName = "6"
        , fullName = "Black 6"
        , value = 0
        , rank = 6
        , suit = Black
        }
    , black_7 =
        { id = "black_7"
        , displayName = "7"
        , fullName = "Black 7"
        , value = 0
        , rank = 7
        , suit = Black
        }
    , black_8 =
        { id = "black_8"
        , displayName = "8"
        , fullName = "Black 8"
        , value = 0
        , rank = 8
        , suit = Black
        }
    , black_9 =
        { id = "black_9"
        , displayName = "9"
        , fullName = "Black 9"
        , value = 0
        , rank = 9
        , suit = Black
        }
    , black_10 =
        { id = "black_10"
        , displayName = "10"
        , fullName = "Black 10"
        , value = 10
        , rank = 10
        , suit = Black
        }
    , black_J =
        { id = "black_J"
        , displayName = "J"
        , fullName = "Black Jack"
        , value = 0
        , rank = 11
        , suit = Black
        }
    , black_Q =
        { id = "black_Q"
        , displayName = "Q"
        , fullName = "Black Queen"
        , value = 0
        , rank = 12
        , suit = Black
        }
    , black_K =
        { id = "black_K"
        , displayName = "K"
        , fullName = "Black King"
        , value = 10
        , rank = 13
        , suit = Black
        }
    , black_A =
        { id = "black_A"
        , displayName = "A"
        , fullName = "Black Ace"
        , value = 0
        , rank = 14
        , suit = Black
        }
    , blue_2 =
        { id = "blue_2"
        , displayName = "2"
        , fullName = "Blue 2"
        , value = 0
        , rank = 2
        , suit = Blue
        }
    , blue_3 =
        { id = "blue_3"
        , displayName = "3"
        , fullName = "Blue 3"
        , value = 0
        , rank = 3
        , suit = Blue
        }
    , blue_4 =
        { id = "blue_4"
        , displayName = "4"
        , fullName = "Blue 4"
        , value = 0
        , rank = 4
        , suit = Blue
        }
    , blue_5 =
        { id = "blue_5"
        , displayName = "5"
        , fullName = "Blue 5"
        , value = 5
        , rank = 5
        , suit = Blue
        }
    , blue_6 =
        { id = "blue_6"
        , displayName = "6"
        , fullName = "Blue 6"
        , value = 0
        , rank = 6
        , suit = Blue
        }
    , blue_7 =
        { id = "blue_7"
        , displayName = "7"
        , fullName = "Blue 7"
        , value = 0
        , rank = 7
        , suit = Blue
        }
    , blue_8 =
        { id = "blue_8"
        , displayName = "8"
        , fullName = "Blue 8"
        , value = 0
        , rank = 8
        , suit = Blue
        }
    , blue_9 =
        { id = "blue_9"
        , displayName = "9"
        , fullName = "Blue 9"
        , value = 0
        , rank = 9
        , suit = Blue
        }
    , blue_10 =
        { id = "blue_10"
        , displayName = "10"
        , fullName = "Blue 10"
        , value = 10
        , rank = 10
        , suit = Blue
        }
    , blue_J =
        { id = "blue_J"
        , displayName = "J"
        , fullName = "Blue Jack"
        , value = 0
        , rank = 11
        , suit = Blue
        }
    , blue_Q =
        { id = "blue_Q"
        , displayName = "Q"
        , fullName = "Blue Queen"
        , value = 0
        , rank = 12
        , suit = Blue
        }
    , blue_K =
        { id = "blue_K"
        , displayName = "K"
        , fullName = "Blue King"
        , value = 10
        , rank = 13
        , suit = Blue
        }
    , blue_A =
        { id = "blue_A"
        , displayName = "A"
        , fullName = "Blue Ace"
        , value = 0
        , rank = 14
        , suit = Blue
        }
    , red_2 =
        { id = "red_2"
        , displayName = "2"
        , fullName = "Red 2"
        , value = 0
        , rank = 2
        , suit = Red
        }
    , red_3 =
        { id = "red_3"
        , displayName = "3"
        , fullName = "Red 3"
        , value = 0
        , rank = 3
        , suit = Red
        }
    , red_4 =
        { id = "red_4"
        , displayName = "4"
        , fullName = "Red 4"
        , value = 0
        , rank = 4
        , suit = Red
        }
    , red_5 =
        { id = "red_5"
        , displayName = "5"
        , fullName = "Red 5"
        , value = 5
        , rank = 5
        , suit = Red
        }
    , red_6 =
        { id = "red_6"
        , displayName = "6"
        , fullName = "Red 6"
        , value = 0
        , rank = 6
        , suit = Red
        }
    , red_7 =
        { id = "red_7"
        , displayName = "7"
        , fullName = "Red 7"
        , value = 0
        , rank = 7
        , suit = Red
        }
    , red_8 =
        { id = "red_8"
        , displayName = "8"
        , fullName = "Red 8"
        , value = 0
        , rank = 8
        , suit = Red
        }
    , red_9 =
        { id = "red_9"
        , displayName = "9"
        , fullName = "Red 9"
        , value = 0
        , rank = 9
        , suit = Red
        }
    , red_10 =
        { id = "red_10"
        , displayName = "10"
        , fullName = "Red 10"
        , value = 10
        , rank = 10
        , suit = Red
        }
    , red_J =
        { id = "red_J"
        , displayName = "J"
        , fullName = "Red Jack"
        , value = 0
        , rank = 11
        , suit = Red
        }
    , red_Q =
        { id = "red_Q"
        , displayName = "Q"
        , fullName = "Red Queen"
        , value = 0
        , rank = 12
        , suit = Red
        }
    , red_K =
        { id = "red_K"
        , displayName = "K"
        , fullName = "Red King"
        , value = 10
        , rank = 13
        , suit = Red
        }
    , red_A =
        { id = "red_A"
        , displayName = "A"
        , fullName = "Red Ace"
        , value = 0
        , rank = 14
        , suit = Red
        }
    }
