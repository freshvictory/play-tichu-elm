module Game.Tichu exposing (TichuLocation(..), TichuPlayer(..), TichuSuit(..), tichuDeck, tichuGame)

import Game.Game as Game


type TichuSuit
    = Special
    | Red
    | Green
    | Black
    | Blue


type TichuPlayer
    = North
    | South
    | East
    | West


type Bomb
    = StraightFlush
    | FourOfAKind


type Combination
    = Single
    | Pair
    | Triple
    | ConsecutivePairs
    | Straight
    | Bomb Bomb


type alias PlayedCombination =
    { id : String
    , player : TichuPlayer
    , type_ : Combination
    }


type TichuLocation
    = Table String
    | Taken TichuPlayer
    | FirstEight TichuPlayer
    | SecondSix TichuPlayer
    | Pass TichuPlayer TichuPlayer


tichuGame : Game.Game TichuSuit TichuPlayer TichuLocation
tichuGame =
    Game.buildGame
        { deck = tichuDeck
        , players = [ North, East, South, West ]
        , dealSpecification = [ ( FirstEight, 8 ), ( SecondSix, 6 ) ]
        }


tichuPlayers : List TichuPlayer
tichuPlayers =
    [ North, East, South, West ]


tichuDeck : Game.Deck TichuSuit
tichuDeck =
    [ { id = "dragon"
      , displayName = "Dragon"
      , value = 25
      , rank = 16
      , suit = Special
      }
    , { id = "dog"
      , displayName = "Dog"
      , value = 0
      , rank = 0
      , suit = Special
      }
    , { id = "phoenix"
      , displayName = "Phoenix"
      , value = -25
      , rank = 15
      , suit = Special
      }
    , { id = "bird"
      , displayName = "Bird"
      , value = 0
      , rank = 1
      , suit = Special
      }
    , { id = "green_2"
      , displayName = "2"
      , value = 0
      , rank = 2
      , suit = Green
      }
    , { id = "green_3"
      , displayName = "3"
      , value = 0
      , rank = 3
      , suit = Green
      }
    , { id = "green_4"
      , displayName = "4"
      , value = 0
      , rank = 4
      , suit = Green
      }
    , { id = "green_5"
      , displayName = "5"
      , value = 5
      , rank = 5
      , suit = Green
      }
    , { id = "green_6"
      , displayName = "6"
      , value = 0
      , rank = 6
      , suit = Green
      }
    , { id = "green_7"
      , displayName = "7"
      , value = 0
      , rank = 7
      , suit = Green
      }
    , { id = "green_8"
      , displayName = "8"
      , value = 0
      , rank = 8
      , suit = Green
      }
    , { id = "green_9"
      , displayName = "9"
      , value = 0
      , rank = 9
      , suit = Green
      }
    , { id = "1green_0"
      , displayName = "10"
      , value = 10
      , rank = 10
      , suit = Green
      }
    , { id = "green_J"
      , displayName = "J"
      , value = 0
      , rank = 11
      , suit = Green
      }
    , { id = "green_Q"
      , displayName = "Q"
      , value = 0
      , rank = 12
      , suit = Green
      }
    , { id = "green_K"
      , displayName = "K"
      , value = 10
      , rank = 13
      , suit = Green
      }
    , { id = "green_A"
      , displayName = "A"
      , value = 0
      , rank = 14
      , suit = Green
      }
    , { id = "black_2"
      , displayName = "2"
      , value = 0
      , rank = 2
      , suit = Black
      }
    , { id = "black_3"
      , displayName = "3"
      , value = 0
      , rank = 3
      , suit = Black
      }
    , { id = "black_4"
      , displayName = "4"
      , value = 0
      , rank = 4
      , suit = Black
      }
    , { id = "black_5"
      , displayName = "5"
      , value = 5
      , rank = 5
      , suit = Black
      }
    , { id = "black_6"
      , displayName = "6"
      , value = 0
      , rank = 6
      , suit = Black
      }
    , { id = "black_7"
      , displayName = "7"
      , value = 0
      , rank = 7
      , suit = Black
      }
    , { id = "black_8"
      , displayName = "8"
      , value = 0
      , rank = 8
      , suit = Black
      }
    , { id = "black_9"
      , displayName = "9"
      , value = 0
      , rank = 9
      , suit = Black
      }
    , { id = "black_10"
      , displayName = "10"
      , value = 10
      , rank = 10
      , suit = Black
      }
    , { id = "black_J"
      , displayName = "J"
      , value = 0
      , rank = 11
      , suit = Black
      }
    , { id = "black_Q"
      , displayName = "Q"
      , value = 0
      , rank = 12
      , suit = Black
      }
    , { id = "black_K"
      , displayName = "K"
      , value = 10
      , rank = 13
      , suit = Black
      }
    , { id = "black_A"
      , displayName = "A"
      , value = 0
      , rank = 14
      , suit = Black
      }
    , { id = "blue_2"
      , displayName = "2"
      , value = 0
      , rank = 2
      , suit = Blue
      }
    , { id = "blue_3"
      , displayName = "3"
      , value = 0
      , rank = 3
      , suit = Blue
      }
    , { id = "blue_4"
      , displayName = "4"
      , value = 0
      , rank = 4
      , suit = Blue
      }
    , { id = "blue_5"
      , displayName = "5"
      , value = 5
      , rank = 5
      , suit = Blue
      }
    , { id = "blue_6"
      , displayName = "6"
      , value = 0
      , rank = 6
      , suit = Blue
      }
    , { id = "blue_7"
      , displayName = "7"
      , value = 0
      , rank = 7
      , suit = Blue
      }
    , { id = "blue_8"
      , displayName = "8"
      , value = 0
      , rank = 8
      , suit = Blue
      }
    , { id = "blue_9"
      , displayName = "9"
      , value = 0
      , rank = 9
      , suit = Blue
      }
    , { id = "blue_10"
      , displayName = "10"
      , value = 10
      , rank = 10
      , suit = Blue
      }
    , { id = "blue_J"
      , displayName = "J"
      , value = 0
      , rank = 11
      , suit = Blue
      }
    , { id = "blue_Q"
      , displayName = "Q"
      , value = 0
      , rank = 12
      , suit = Blue
      }
    , { id = "blue_K"
      , displayName = "K"
      , value = 10
      , rank = 13
      , suit = Blue
      }
    , { id = "blue_A"
      , displayName = "A"
      , value = 0
      , rank = 14
      , suit = Blue
      }
    , { id = "red_2"
      , displayName = "2"
      , value = 0
      , rank = 2
      , suit = Red
      }
    , { id = "red_3"
      , displayName = "3"
      , value = 0
      , rank = 3
      , suit = Red
      }
    , { id = "red_4"
      , displayName = "4"
      , value = 0
      , rank = 4
      , suit = Red
      }
    , { id = "red_5"
      , displayName = "5"
      , value = 5
      , rank = 5
      , suit = Red
      }
    , { id = "red_6"
      , displayName = "6"
      , value = 0
      , rank = 6
      , suit = Red
      }
    , { id = "red_7"
      , displayName = "7"
      , value = 0
      , rank = 7
      , suit = Red
      }
    , { id = "red_8"
      , displayName = "8"
      , value = 0
      , rank = 8
      , suit = Red
      }
    , { id = "red_9"
      , displayName = "9"
      , value = 0
      , rank = 9
      , suit = Red
      }
    , { id = "red_10"
      , displayName = "10"
      , value = 10
      , rank = 10
      , suit = Red
      }
    , { id = "red_J"
      , displayName = "J"
      , value = 0
      , rank = 11
      , suit = Red
      }
    , { id = "red_Q"
      , displayName = "Q"
      , value = 0
      , rank = 12
      , suit = Red
      }
    , { id = "red_K"
      , displayName = "K"
      , value = 10
      , rank = 13
      , suit = Red
      }
    , { id = "red_A"
      , displayName = "A"
      , value = 0
      , rank = 14
      , suit = Red
      }
    ]
