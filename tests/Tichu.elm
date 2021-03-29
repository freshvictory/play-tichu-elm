module Tichu exposing (..)

import Expect
import Game.Cards as Cards
import Game.Tichu exposing (..)
import Test exposing (..)


game : Cards.PlayableDeck Suit
game =
    Cards.buildDeck deckDefinition


dealing : Test
dealing =
    describe "Tichu dealing"
        [ test "Deals the approprate number of cards to each position per player" <|
            \_ ->
                let
                    deal =
                        game.deal game.deck
                in
                deal
                    |> Expect.all
                        (List.map
                            (\( location, count ) ->
                                \d ->
                                    Expect.equal
                                        count
                                        (List.length (Cards.selectFrom location d))
                            )
                            deckDefinition.deal
                        )
        ]


combinations : Test
combinations =
    let
        combinationFromIds =
            \cardIds ->
                determineCombination <|
                    List.map (\id -> id cardDefinition) cardIds

        expectEquals =
            \expectedCombination cardIds ->
                let
                    expectedCards =
                        List.map (\id -> id cardDefinition) cardIds
                in
                Expect.all
                    [ \( combination, _ ) -> Expect.equal expectedCombination combination
                    , \( _, cards ) -> Expect.equalLists expectedCards cards
                    ]

        expect =
            \combination cardIds ->
                combinationFromIds cardIds
                    |> expectEquals combination cardIds
    in
    describe "Tichu combinations"
        [ test "The Dog" <|
            \_ ->
                expect Dog [ .dog ]
        , describe "Single cards"
            (List.map
                (\c ->
                    test ("Parses " ++ c.fullName) <|
                        \_ ->
                            determineCombination [ c ]
                                |> Expect.equal ( Single, [ c ] )
                )
                (List.filter (\c -> c /= cardDefinition.dog) deckDefinition.deck)
            )
        , describe "Pairs"
            [ test "Normal pair" <|
                \_ ->
                    expect Pair [ .green_2, .red_2 ]
            , describe "Pairs with phoenix"
                [ test "Normal card with phoenix" <|
                    \_ ->
                        expect Pair [ .green_2, .phoenix ]
                , test "Normal card with phoenix first should order phoenix last" <|
                    \_ ->
                        combinationFromIds [ .phoenix, .green_2 ]
                            |> expectEquals Pair [ .green_2, .phoenix ]
                ]
            , describe "Special cards"
                [ test "Dragon can't be used in a pair" <|
                    \_ ->
                        expect Irregular [ .green_2, .dragon ]
                , test "Dog can't be used in a pair" <|
                    \_ ->
                        expect Irregular [ .dog, .green_2 ]
                , test "Phoenix can't be used with dragon" <|
                    \_ ->
                        expect Irregular [ .phoenix, .dragon ]
                , test "Phoenix can't be used with dog" <|
                    \_ ->
                        expect Irregular [ .dog, .phoenix ]
                , test "Bird can't be used in a pair" <|
                    \_ ->
                        expect Irregular [ .bird, .green_2 ]
                , test "Bird with phoenix is allowed" <|
                    \_ ->
                        expect Pair [ .bird, .phoenix ]
                ]
            ]
        , describe "Triples"
            [ test "Normal triple" <|
                \_ ->
                    expect Triple [ .green_2, .red_2, .black_2 ]
            , describe "Triples with phoenix"
                [ test "Normal cards with phoenix" <|
                    \_ ->
                        expect Triple [ .green_2, .red_2, .phoenix ]
                , test "Normal cards with phoenix first should order phoenix last" <|
                    \_ ->
                        combinationFromIds [ .phoenix, .green_2, .red_2 ]
                            |> expectEquals Triple [ .green_2, .red_2, .phoenix ]
                , test "Normal cards with phoenix second should order phoenix last" <|
                    \_ ->
                        combinationFromIds [ .green_2, .phoenix, .red_2 ]
                            |> expectEquals Triple [ .green_2, .red_2, .phoenix ]
                ]
            , describe "Special cards"
                [ test "Dragon can't be used in a triple" <|
                    \_ ->
                        expect Irregular [ .green_2, .red_2, .dragon ]
                , test "Dog can't be used in a triple" <|
                    \_ ->
                        expect Irregular [ .dog, .green_2, .red_2 ]
                , test "Bird can't be used in a triple" <|
                    \_ ->
                        expect Irregular [ .bird, .green_2, .red_2 ]
                , test "Dog, bird, and dragon aren't a triple" <|
                    \_ ->
                        expect Irregular [ .dog, .bird, .dragon ]
                , describe "Phoenix can't be used with a special card"
                    [ test "Phoenix can't be used with dragon" <|
                        \_ ->
                            expect Irregular [ .green_2, .phoenix, .dragon ]
                    , test "Phoenix can't be used with dog" <|
                        \_ ->
                            expect Irregular [ .dog, .green_2, .phoenix ]
                    , test "Phoenix can't be used with bird" <|
                        \_ ->
                            expect Irregular [ .bird, .green_2, .phoenix ]
                    , test "Phoenix can't be used with bird and dog" <|
                        \_ ->
                            expect Irregular [ .dog, .bird, .phoenix ]
                    , test "Phoenix can't be used with bird and dragon" <|
                        \_ ->
                            expect Irregular [ .bird, .phoenix, .dragon ]
                    , test "Phoenix can't be used with dog and dragon" <|
                        \_ ->
                            expect Irregular [ .dog, .phoenix, .dragon ]
                    ]
                ]
            ]
        , describe "Consecutive pairs"
            [ describe "Normal consecutive pairs"
                [ test "Two consecutive pairs" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .green_2
                            , .red_2
                            , .black_3
                            , .blue_3
                            ]
                , test "Three consecutive pairs" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .green_2
                            , .red_2
                            , .black_3
                            , .blue_3
                            , .red_4
                            , .blue_4
                            ]
                , test "Four consecutive pairs" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .green_2
                            , .red_2
                            , .black_3
                            , .blue_3
                            , .red_4
                            , .blue_4
                            , .green_5
                            , .red_5
                            ]
                , test "Five consecutive pairs" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .green_2
                            , .red_2
                            , .black_3
                            , .blue_3
                            , .red_4
                            , .blue_4
                            , .green_5
                            , .red_5
                            , .black_6
                            , .blue_6
                            ]
                , test "Six consecutive pairs" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .green_2
                            , .red_2
                            , .black_3
                            , .blue_3
                            , .red_4
                            , .blue_4
                            , .green_5
                            , .red_5
                            , .black_6
                            , .blue_6
                            , .red_7
                            , .green_7
                            ]
                , test "Seven consecutive pairs" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .green_2
                            , .red_2
                            , .black_3
                            , .blue_3
                            , .red_4
                            , .blue_4
                            , .green_5
                            , .red_5
                            , .black_6
                            , .blue_6
                            , .red_7
                            , .green_7
                            , .black_8
                            , .blue_8
                            ]
                , test "One invalid card in four" <|
                    \_ ->
                        expect Irregular
                            [ .green_2
                            , .red_2
                            , .green_3
                            , .red_4
                            ]
                , test "One invalid card in six" <|
                    \_ ->
                        expect Irregular
                            [ .green_2
                            , .red_2
                            , .green_3
                            , .blue_3
                            , .green_4
                            , .red_4
                            , .black_4
                            , .blue_5
                            ]
                , test "Pairs aren't consecutive" <|
                    \_ ->
                        expect Irregular
                            [ .green_2
                            , .red_2
                            , .green_4
                            , .red_4
                            ]
                ]
            , describe "Consecutive pairs with phoenix"
                [ describe "Valid consecutive pairs with phoenix"
                    [ test "Two consecutive pairs, phoenix in first pair" <|
                        \_ ->
                            expect ConsecutivePairs
                                [ .green_2
                                , .phoenix
                                , .green_3
                                , .red_3
                                ]
                    , test "Two consecutive pairs, phoenix in second pair" <|
                        \_ ->
                            expect ConsecutivePairs
                                [ .green_2
                                , .red_2
                                , .green_3
                                , .phoenix
                                ]
                    , test "Three consecutive pairs, phoenix in middle pair" <|
                        \_ ->
                            expect ConsecutivePairs
                                [ .green_2
                                , .red_2
                                , .green_3
                                , .phoenix
                                , .green_4
                                , .red_4
                                ]
                    ]
                , describe "Invalid consecutive pairs with phoenix"
                    [ test "Pairs aren't consecutive" <|
                        \_ ->
                            expect Irregular
                                [ .green_2
                                , .red_2
                                , .green_4
                                , .phoenix
                                ]
                    , test "Phoenix is needed more than once" <|
                        \_ ->
                            expect Irregular
                                [ .green_2
                                , .red_2
                                , .green_3
                                , .red_4
                                , .red_5
                                , .phoenix
                                ]
                    ]
                ]
            , describe "Special cards"
                [ test "Dragon with phoenix isn't valid" <|
                    \_ ->
                        expect Irregular
                            [ .green_A
                            , .red_A
                            , .phoenix
                            , .dragon
                            ]
                , test "Dog with phoenix isn't valid" <|
                    \_ ->
                        expect Irregular
                            [ .dog
                            , .green_2
                            , .red_2
                            , .phoenix
                            ]
                , test "Bird with phoeinx is valid" <|
                    \_ ->
                        expect ConsecutivePairs
                            [ .bird
                            , .phoenix
                            , .green_2
                            , .red_2
                            ]
                ]
            ]
        , describe "Full houses"
            [ describe "Normal full houses"
                [ test "Triple high" <|
                    \_ ->
                        expect FullHouse
                            [ .green_8
                            , .red_8
                            , .blue_8
                            , .red_3
                            , .black_3
                            ]
                , test "Pair high" <|
                    \_ ->
                        expect FullHouse
                            [ .blue_2
                            , .black_2
                            , .red_2
                            , .green_A
                            , .red_A
                            ]
                ]
            , describe "Full houses with phoenix"
                [ test "Phoenix attaches to higher pair" <|
                    \_ ->
                        expect FullHouse
                            [ .red_8
                            , .blue_8
                            , .phoenix
                            , .green_2
                            , .black_2
                            ]
                , test "Phoenix can fill in higher single" <|
                    \_ ->
                        expect FullHouse
                            [ .red_8
                            , .blue_8
                            , .black_8
                            , .green_K
                            , .phoenix
                            ]
                , test "Phoenix can fill in lower single" <|
                    \_ ->
                        expect FullHouse
                            [ .red_8
                            , .blue_8
                            , .black_8
                            , .green_2
                            , .phoenix
                            ]
                , test "Phoenix can't be used with a four of a kind bomb" <|
                    \_ ->
                        expect Irregular
                            [ .red_8
                            , .blue_8
                            , .black_8
                            , .green_8
                            , .phoenix
                            ]
                , test "Phoenix can be used with the bird" <|
                    \_ ->
                        expect FullHouse
                            [ .red_8
                            , .blue_8
                            , .black_8
                            , .bird
                            , .phoenix
                            ]
                ]
            ]
        , describe "Straights"
            [ describe "Normal straights"
                [ test "Five card straight" <|
                    \_ ->
                        expect Straight
                            [ .green_2
                            , .red_3
                            , .red_4
                            , .green_5
                            , .blue_6
                            ]
                , test "Six card straight" <|
                    \_ ->
                        expect Straight
                            [ .green_2
                            , .red_3
                            , .red_4
                            , .green_5
                            , .blue_6
                            , .black_7
                            ]
                , test "Four card straight" <|
                    \_ ->
                        expect Irregular
                            [ .green_2
                            , .red_3
                            , .red_4
                            , .green_5
                            ]
                , test "Straight with gap" <|
                    \_ ->
                        expect Irregular
                            [ .green_2
                            , .green_4
                            , .red_5
                            , .blue_6
                            , .black_7
                            ]
                ]
            , describe "Straights with phoenix"
                [ test "Phoenix in middle" <|
                    \_ ->
                        expect Straight
                            [ .green_2
                            , .red_3
                            , .phoenix
                            , .red_5
                            , .green_6
                            ]
                , test "Phoenix at end" <|
                    \_ ->
                        expect Straight
                            [ .green_2
                            , .red_3
                            , .green_4
                            , .red_5
                            , .phoenix
                            ]
                , test "Phoenix at the beginning if four card straight ends in ace" <|
                    \_ ->
                        expect Straight
                            [ .phoenix
                            , .green_J
                            , .blue_Q
                            , .black_K
                            , .green_A
                            ]
                , test "Phoenix goes to beginning if five card straight ends in ace" <|
                    \_ ->
                        combinationFromIds
                            [ .red_10
                            , .green_J
                            , .blue_Q
                            , .black_K
                            , .green_A
                            , .phoenix
                            ]
                            |> expectEquals Straight
                                [ .phoenix
                                , .red_10
                                , .green_J
                                , .blue_Q
                                , .black_K
                                , .green_A
                                ]
                ]
            , describe "Special cards"
                [ test "Bird works in straight" <|
                    \_ ->
                        expect Straight
                            [ .bird
                            , .green_2
                            , .red_3
                            , .green_4
                            , .black_5
                            ]
                , test "Bird works with phoenix" <|
                    \_ ->
                        expect Straight
                            [ .bird
                            , .phoenix
                            , .green_3
                            , .red_4
                            , .black_5
                            ]
                , test "Dog does not work in straight" <|
                    \_ ->
                        expect Irregular
                            [ .dog
                            , .bird
                            , .green_2
                            , .red_3
                            , .black_4
                            , .blue_5
                            ]
                , test "Dragon does not work in straight" <|
                    \_ ->
                        expect Irregular
                            [ .green_J
                            , .blue_Q
                            , .black_K
                            , .red_A
                            , .dragon
                            ]
                , test "Dragon does not work in straight with phoenix" <|
                    \_ ->
                        expect Irregular
                            [ .green_J
                            , .blue_Q
                            , .black_K
                            , .red_A
                            , .phoenix
                            , .dragon
                            ]
                ]
            ]
        , describe "Bombs"
            [ describe "Four of a kind"
                [ test "Valid four of a kind" <|
                    \_ ->
                        expect (Bomb FourOfAKind)
                            [ .red_4
                            , .green_4
                            , .blue_4
                            , .black_4
                            ]
                , test "Phoenix can't make a four of a kind bomb" <|
                    \_ ->
                        expect Irregular
                            [ .red_4
                            , .blue_4
                            , .black_4
                            , .phoenix
                            ]
                , test "All the special cards is not a bomb" <|
                    \_ ->
                        expect Irregular
                            [ .dog
                            , .bird
                            , .phoenix
                            , .dragon
                            ]
                ]
            , describe "Straight flush"
                [ test "Valid straight flush" <|
                    \_ ->
                        expect (Bomb StraightFlush)
                            [ .green_2
                            , .green_3
                            , .green_4
                            , .green_5
                            , .green_6
                            ]
                , test "Phoenix can't make a straight flush" <|
                    \_ ->
                        expect Straight
                            [ .green_2
                            , .green_3
                            , .green_4
                            , .phoenix
                            , .green_6
                            ]
                ]
            ]
        ]
