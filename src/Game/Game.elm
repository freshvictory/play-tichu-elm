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


type alias Deck s =
    List (Card s)


type alias GameDeck suit l =
    List (CardInPlay (Card suit) l)


type alias CardInPlay c l =
    { definition : c
    , location : l
    }


type alias Deal player location =
    List ( player -> location, Int )


type alias GameDefinition suit player location =
    { deck : Deck suit
    , players : List player
    , dealSpecification : Deal player location
    }


type alias Game suit player location =
    { players : List player
    , deal : Deck suit -> GameDeck suit location
    , shuffledDeckGenerator : Random.Generator (Deck suit)
    }


buildGame : GameDefinition suit player location -> Game suit player location
buildGame definition =
    { players = definition.players
    , deal = deal definition.dealSpecification definition.players
    , shuffledDeckGenerator = shuffle definition.deck
    }


deal : Deal player location -> List player -> Deck suit -> GameDeck suit location
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
