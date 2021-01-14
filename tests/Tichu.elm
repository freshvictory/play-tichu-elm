module Tichu exposing (..)

import Expect
import Game.Tichu exposing (TichuLocation(..), TichuPlayer(..), tichuDeck, tichuGame)
import Test exposing (..)


tichuDealing : Test
tichuDealing =
    describe "Tichu dealing"
        [ test "Will deal all cards" <|
            \_ ->
                Expect.equal (List.length tichuDeck) (List.length (tichuGame.deal tichuDeck))
        , test "Deals the approprate number of cards to each position per player" <|
            \_ ->
                let
                    deal =
                        tichuGame.deal tichuDeck

                    cardsPerPlayer =
                        List.length tichuDeck // List.length tichuGame.players
                in
                deal
                    |> Expect.all
                        [ \d -> Expect.equal
                            { north = 8, south = 8, east = 8, west = 8 }
                            (List.foldl
                                (\card counts ->
                                    case card.location of
                                        FirstEight p ->
                                            case p of
                                                North ->
                                                    { counts | north = counts.north + 1 }

                                                South ->
                                                    { counts | south = counts.south + 1 }

                                                East ->
                                                    { counts | east = counts.east + 1 }

                                                West ->
                                                    { counts | west = counts.west + 1 }

                                        _ ->
                                            counts
                                )
                                { north = 0, south = 0, east = 0, west = 0 }
                                d
                            )
                        , \d -> Expect.equal
                            { north = 6, south = 6, east = 6, west = 6 }
                            (List.foldl
                                (\card counts ->
                                    case card.location of
                                        SecondSix p ->
                                            case p of
                                                North ->
                                                    { counts | north = counts.north + 1 }

                                                South ->
                                                    { counts | south = counts.south + 1 }

                                                East ->
                                                    { counts | east = counts.east + 1 }

                                                West ->
                                                    { counts | west = counts.west + 1 }

                                        _ ->
                                            counts
                                )
                                { north = 0, south = 0, east = 0, west = 0 }
                                d
                            )
                        ]
                
        ]
