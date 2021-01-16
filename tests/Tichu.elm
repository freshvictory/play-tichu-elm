module Tichu exposing (..)

import Expect
import Game.Game exposing (Location(..), PlayerLocation(..), deal)
import Game.Tichu exposing (TichuPlayer(..), tichuDealSpecification, tichuDeck, tichuGame)
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
                        (List.map
                            (\( location, count ) ->
                                \d ->
                                    Expect.equal
                                        { north = count, south = count, east = count, west = count }
                                        (List.foldl
                                            (\card counts ->
                                                if card.location == location North then
                                                    { counts | north = counts.north + 1 }

                                                else if card.location == location South then
                                                    { counts | south = counts.south + 1 }

                                                else if card.location == location East then
                                                    { counts | east = counts.east + 1 }

                                                else if card.location == location West then
                                                    { counts | west = counts.west + 1 }

                                                else
                                                    counts
                                            )
                                            { north = 0, south = 0, east = 0, west = 0 }
                                            d
                                        )
                            )
                            tichuDealSpecification
                        )
        , test "Shorter deals are split between players and the deck" <|
            \_ ->
                Expect.equal
                    { players = 32, deck = 24 }
                    (List.foldl
                        (\card counts ->
                            if card.location == Deck then
                                { counts | deck = counts.deck + 1 }

                            else
                                { counts | players = counts.players + 1 }
                        )
                        { players = 0, deck = 0 }
                        (deal [ ( PlayerLocation Hand, 8 ) ] [ North, South, East, West ] tichuDeck)
                    )
        ]
