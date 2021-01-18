module Tichu exposing (..)

import Expect
import Game.Cards as Cards
import Game.Tichu as Tichu
import Test exposing (..)


game : Cards.Game Tichu.Suit Tichu.Action
game =
    Cards.buildGame Tichu.gameDefinition


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
                            Tichu.gameDefinition.deal
                        )
        ]
