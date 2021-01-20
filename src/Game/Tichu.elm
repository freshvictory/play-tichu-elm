module Game.Tichu exposing (Action(..), Combination(..), Suit(..), gameDefinition)

import Game.Cards as Cards exposing (Cards)


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
    | ConsecutivePairs (List ( Card, Card ))
    | Straight (List Card)
    | Bomb Bomb


type Action
    = Pass Cards.Player Card Cards.Player
    | PickUp Cards.Player
    | Play Cards.Player Combination
    | EveryonePickUp


deal : Cards.Deal
deal =
    [ ( Cards.PlayerLocation Cards.Hand Cards.North, 8 )
    , ( Cards.PlayerLocation Cards.Hand Cards.South, 8 )
    , ( Cards.PlayerLocation Cards.Hand Cards.East, 8 )
    , ( Cards.PlayerLocation Cards.Hand Cards.West, 8 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) Cards.North, 6 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) Cards.South, 6 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) Cards.East, 6 )
    , ( Cards.PlayerLocation (Cards.InFront Cards.FaceDown) Cards.West, 6 )
    ]


gameDefinition : Cards.GameDefinition Suit Action
gameDefinition =
    { deck = deck
    , deal = deal
    , act = act
    }


act : Action -> Cards.ActionResult Suit
act action =
    case action of
        PickUp player ->
            Cards.MoveCards
                (\card ->
                    card.location == Cards.PlayerLocation (Cards.InFront Cards.FaceDown) player
                )
                (Cards.PlayerLocation Cards.Hand player)

        Pass passer cardToPass passee ->
            Cards.MoveCards
                (\card -> card.definition.id == cardToPass.id)
                (Cards.PlayerLocation (Cards.PassingTo passee) passer)

        Play player combination ->
            let
                cardsInCombination =
                    case combination of
                        Single singleCard ->
                            [ singleCard ]

                        Pair ( firstCard, secondCard ) ->
                            [ firstCard, secondCard ]

                        Triple ( firstCard, secondCard, thirdCard ) ->
                            [ firstCard, secondCard, thirdCard ]

                        ConsecutivePairs cards ->
                            List.concatMap
                                (\( firstCard, secondCard ) -> [ firstCard, secondCard ])
                                cards

                        Straight cards ->
                            cards

                        Bomb bomb ->
                            case bomb of
                                StraightFlush cards ->
                                    cards

                                FourOfAKind cards ->
                                    cards
            in
            Cards.PlayCards
                player
                cardsInCombination
                Cards.Table

        EveryonePickUp ->
            Cards.MapCards
                (\card ->
                    case card.location of
                        Cards.PlayerLocation (Cards.PassingTo passee) player ->
                            Cards.PlayerLocation Cards.Hand passee

                        _ ->
                            card.location
                )


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
