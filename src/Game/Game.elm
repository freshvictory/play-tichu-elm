module Game.Game exposing (..)

import Array
import Random
import Random.Array


type alias Card s =
    { id : String
    , displayName : String
    , rank : Int
    , value : Int
    , suit : s
    }


type CardState
    = FaceUp
    | FaceDown


type PlayerLocation player
    = Hand
    | InFront CardState
    | Taken
    | Table
    | PassedFrom player


type Location player
    = Deck
    | PlayerLocation (PlayerLocation player) player


type alias Deck s =
    List (Card s)


type alias GameDeck suit player =
    List (CardInPlay (Card suit) player)


type alias CardInPlay c player =
    { definition : c
    , location : Location player
    }


type alias Deal player =
    List ( player -> Location player, Int )


type alias GameDefinition suit player =
    { deck : Deck suit
    , players : List player
    , dealSpecification : Deal player
    }


type alias Game suit player =
    { players : List player
    , deal : Deck suit -> GameDeck suit player
    , shuffledDeckGenerator : Random.Generator (Deck suit)
    }


buildGame : GameDefinition suit player -> Game suit player
buildGame definition =
    { players = definition.players
    , deal = deal definition.dealSpecification definition.players
    , shuffledDeckGenerator = shuffle definition.deck
    }


deal : Deal player -> List player -> Deck suit -> GameDeck suit player
deal spec players deck =
    let
        numberOfCards =
            List.length deck

        numberOfPlayers =
            List.length players

        playerMap =
            List.concat (List.repeat (numberOfCards // numberOfPlayers) players)

        locations =
            List.concatMap
                (\( location, count ) ->
                    List.repeat count location
                )
                spec

        locationMap =
            List.concat (List.repeat (numberOfCards // List.length locations) locations)

        deckToDealToPlayer =
            List.map3 (\card location player -> ( card, location, player )) deck locationMap playerMap
    in
    List.map
        (\( card, location, player ) ->
            { definition = card
            , location = location player
            }
        )
        deckToDealToPlayer


shuffle : Deck suit -> Random.Generator (Deck suit)
shuffle deck =
    let
        array =
            Array.fromList deck
    in
    Random.Array.shuffle array
        |> Random.andThen (\arr -> Random.constant (Array.toList arr))
